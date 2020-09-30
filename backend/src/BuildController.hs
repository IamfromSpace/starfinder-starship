{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module BuildController where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
       (ProxyRequest(..), authorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
       (ProxyResponse, setHeader, applicationJson, badRequest400, conflict409,
        forbidden403, methodNotAllowed405, notFound404, notImplemented501,
        ok200, preconditionFailed412, response, textPlain, unauthorized401)
import Authorizer (Authorizer)
import BuildAuthorizer (AuthError(..), authorizeUser)
import qualified BuildRepo as BR (DynamoBuildRepoError(..))
import BuildService (Action, BuildService)
import qualified BuildService as BS
import Control.Monad ((>=>))
import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode')
import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Error.VersionMismatch (VersionMismatch(..))
import Lib (ETagged(..), OwnedBy(..))
import Network.HTTP.Types.URI (decodePathSegments)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, local, runReader)
import Prelude hiding (lookup)
import Starfinder.Starship.Build (Build)
import qualified Starfinder.Starship.Build as B (Build(name))
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)
import Text.Read (readMaybe)

newtype UserId = UserId
    { getUserId :: Text
    } deriving (Show)

instance FromJSON UserId where
    parseJSON (Object v) =
        UserId <$>
        (v .: "claims" >>=
         (\case
              Object v2 -> v2 .: "sub"
              _ -> fail "JWT claims was not object!"))
    parseJSON _ = fail "API Gateway Authorizer value was not an object!"

httpHandler ::
       Member (BuildService a) r
    => Member (Reader (Maybe Text)) r =>
           ProxyRequest UserId -> Sem r ProxyResponse
httpHandler ProxyRequest {path, requestContext, body, httpMethod, headers} =
    local (\_ -> getUserId <$> authorizer requestContext) $
    case parseApiGatweayPath path of
        Just (UserBuilds userId) ->
            case httpMethod of
                "POST" ->
                    case eitherDecode' body of
                        Right (build :: Build Text ReferencedWeapon Text) ->
                            done <$> (BS.saveNewBuild $ OwnedBy userId build)
                        Left err -> return (invalidJson err)
                -- TODO: Return the link too
                "GET" -> list <$> BS.getBuildsByOwner userId
                _ -> return methodNotAllowed
        Just (UserBuild userId name) ->
            case httpMethod of
                "GET" -> got <$> BS.getBuild userId name
                "PUT" ->
                    case (eitherDecode' body, getAndParseETag headers) of
                        (Right (build :: Build Text ReferencedWeapon Text), Right expectedETag) ->
                            if B.name build == name
                            -- TODO: Just in general this speaks to the oddity
                            -- of names.  While we need to know the name on
                            -- POST, there are many contexts where we don't,
                            -- for example, why would a Starship with status
                            -- name its build?  The starship is already named.
                            -- Perhaps `Named Text a` is a type.
                                then (done . fromRight) <$>
                                     BS.updateBuild
                                         userId
                                         name
                                         expectedETag
                                         (const $ pure $ OwnedBy userId build)
                                else return badRequest
                        (Right (build :: Build Text ReferencedWeapon Text), Left NotPresent) ->
                            if B.name build == name
                                then done <$>
                                     (BS.saveNewBuild $ OwnedBy userId build)
                                else return badRequest
                        (_, Left Invalid) ->
                            return badRequest -- clearly didn't get this from us, so they're doing something wrong.
                        (Left err, _) -> return (invalidJson err)
                "PATCH" -> return notImplemented
                _ -> return methodNotAllowed
        _ -> return notFound

httpAuthorizer ::
       Sem ((Authorizer Action) ': (Error AuthError) ': (Reader (Maybe Text)) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpAuthorizer =
    runReader Nothing .
    fmap
        (either
             (\case
                  Unidentified -> unauthorized
                  Forbidden -> forbidden)
             id) .
    runError . authorizeUser

httpDynamoBuildRepoErrorHandler ::
       Sem ((Error BR.DynamoBuildRepoError) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpDynamoBuildRepoErrorHandler =
    let handler =
            \case
                BR.AlreadyExists ->
                    response
                        conflict409
                        (textPlain "User already has a ship with that name.")
                BR.DoesNotExist -> notFound
    in fmap (either handler id) . runError

httpBuildServiceErrorHandler ::
       Sem ((Error BS.BuildServiceError) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpBuildServiceErrorHandler =
    let handler =
            \case
                BS.StaticValidationError errors ->
                    response
                        badRequest400
                        -- TODO: Ideally these are formatted so they're more
                        -- actionable (both Create and Update)
                        (textPlain $ pack $ show errors)
                BS.IllegalChange errors ->
                    response
                        conflict409
                        -- TODO: format these so their actionable
                        (textPlain $ pack $ show errors)
    in fmap (either handler id) . runError

httpVersionMismatchHandler ::
       Sem ((Error (VersionMismatch (OwnedBy (Build Text ReferencedWeapon Text)))) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpVersionMismatchHandler =
    let handler =
            \case
                VersionMismatch (ETagged eTag (OwnedBy _ build)) ->
                    setHeader "ETag" (hashToETagValue eTag) $
                    response preconditionFailed412 (applicationJson build)
    in fmap (either handler id) . runError

done :: Int -> ProxyResponse
done eTag =
    setHeader "ETag" (hashToETagValue eTag) $ response ok200 (textPlain "Done")

got :: ETagged (OwnedBy (Build Text ReferencedWeapon Text)) -> ProxyResponse
got (ETagged eTag (OwnedBy _ build)) =
    setHeader "ETag" (hashToETagValue eTag) $
    response ok200 (applicationJson build)

list :: [Text] -> ProxyResponse
list names = response ok200 (applicationJson names)

invalidJson :: String -> ProxyResponse
invalidJson err =
    response badRequest400 (textPlain (pack err))

unauthorized :: ProxyResponse
unauthorized =
    response unauthorized401 (textPlain "Unauthorized")

badRequest :: ProxyResponse
badRequest = response badRequest400 (textPlain "Bad Request")

notImplemented :: ProxyResponse
notImplemented =
    response
        notImplemented501
        (textPlain "Not Yet Implemented")

notFound :: ProxyResponse
notFound = response notFound404 (textPlain "Not Found")

methodNotAllowed :: ProxyResponse
methodNotAllowed =
    response
        methodNotAllowed405
        (textPlain "Method Not Allowed")

forbidden :: ProxyResponse
forbidden = response forbidden403 (textPlain "Forbidden")

hashToETagValue :: Int -> Text
hashToETagValue = pack . show . show

eTagValueToHash :: Text -> Maybe Int
eTagValueToHash = readMaybe . unpack >=> readMaybe

data ETagValidity
    = NotPresent
    | Invalid

getAndParseETag :: HashMap (CI Text) Text -> Either ETagValidity Int
getAndParseETag =
    let getETag = maybe (Left NotPresent) Right . lookup (mk "If-Match")
        parseETag = maybe (Left Invalid) Right . eTagValueToHash
    in getETag >=> parseETag

data ValidPath
    -- /resources/users/${Text}/builds
    = UserBuilds Text
    -- /resources/users/${Text}/builds/${Text}
    | UserBuild Text
                Text
    deriving (Show)

parseApiGatweayPath :: Text -> Maybe (ValidPath)
parseApiGatweayPath =
    let parseSegments =
            \case
                ["resources", "users", u, "builds"] -> Just $ UserBuilds u
                ["resources", "users", u, "builds", b] -> Just $ UserBuild u b
                _ -> Nothing
    in parseSegments . filter (/= "") . decodePathSegments . encodeUtf8

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "Attempted to fromRight a Left!"
