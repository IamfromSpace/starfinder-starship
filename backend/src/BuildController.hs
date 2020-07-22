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
       (ProxyResponse(..), applicationJson, badRequest400, conflict409,
        forbidden403, methodNotAllowed405, notFound404, notImplemented501,
        ok200, preconditionFailed412, textPlain, unauthorized401)
import Authorizer (Authorizer)
import BuildAuthorizer (Forbidden, authorizeUser)
import qualified BuildRepo as BR (DynamoBuildRepoError(..))
import BuildService (Action, BuildService)
import qualified BuildService as BS
import Control.Monad ((>=>))
import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode')
import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, fromList, lookup)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Lib (ETagged(..), OwnedBy(..))
import Network.HTTP.Types.URI (decodePathSegments)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError)
import Polysemy.Reader (Reader, runReader)
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
        UserId <$> (v .: "claims" >>= (\(Object v2) -> v2 .: "sub"))

-- TODO: These all seem to form a pattern where it's really just transforming
-- the Service result into ProxyResponse.  I'm still not totally convinced
-- that's the abstraction though.
handleCreate ::
       Member (BuildService a) r
    => Text
    -> Build Text ReferencedWeapon Text
    -> Sem r ProxyResponse
handleCreate userId build = do
    eTag <- BS.saveNewBuild $ OwnedBy userId build
    return
        (ProxyResponse
             ok200
             (fromList [("ETag", hashToETagValue eTag)])
             mempty
             (textPlain "Done"))

handleGet :: Member (BuildService a) r => Text -> Text -> Sem r ProxyResponse
handleGet userId name = do
    (ETagged eTag (OwnedBy _ build)) <- BS.getBuild userId name
    return
        (ProxyResponse
             ok200
             (fromList [("ETag", hashToETagValue eTag)])
             mempty
             (applicationJson build))

handlePut ::
       Member (BuildService a) r
    => Text
    -> Text
    -> Int
    -> Build Text ReferencedWeapon Text
    -> Sem r ProxyResponse
handlePut userId name expectedETag build = do
    -- impossible to fail the transform
    eTag <- fromRight <$>
        BS.updateBuild
            userId
            name
            expectedETag
            (const $ pure $ OwnedBy userId build)
    return
        (ProxyResponse
             ok200
             (fromList [("ETag", hashToETagValue eTag)])
             mempty
             (textPlain "Done"))

httpHandler ::
       Member (BuildService a) r => ProxyRequest UserId -> Sem r ProxyResponse
httpHandler pr@(ProxyRequest {path, requestContext, body, httpMethod, headers}) =
    case parseApiGatweayPath path of
        Just (UserBuilds userId) ->
            case httpMethod of
                "POST" ->
                    case eitherDecode' body of
                        Right (build :: Build Text ReferencedWeapon Text) ->
                            handleCreate userId build
                        Left err -> return (invalidJson err)
                _ -> return methodNotAllowed
        Just (UserBuild userId name) ->
            case httpMethod of
                "GET" -> handleGet userId name
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
                                then handlePut userId name expectedETag build
                                else return badRequest
                        (Right (build :: Build Text ReferencedWeapon Text), Left NotPresent) ->
                            if B.name build == name
                                then handleCreate userId build
                                else return badRequest
                        (_, Left Invalid) ->
                            return badRequest -- clearly didn't get this from us, so they're doing something wrong.
                        (Left err, _) -> return (invalidJson err)
                "PATCH" -> return notImplemented
                _ -> return methodNotAllowed
        _ -> return notFound

httpAuthorizer ::
       ProxyRequest UserId
    -> Sem ((Authorizer Action) ': (Error Forbidden) ': (Reader Text) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpAuthorizer ProxyRequest {requestContext} =
    runReader (getUserId (fromJust (authorizer requestContext))) .
    fmap (either (const forbidden) id) . runError . authorizeUser

httpDynamoBuildRepoErrorHandler ::
       Sem ((Error BR.DynamoBuildRepoError) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpDynamoBuildRepoErrorHandler =
    let handler =
            \case
                BR.AlreadyExists ->
                    ProxyResponse
                        conflict409
                        mempty
                        mempty
                        (textPlain "User already has a ship with that name.")
                BR.DoesNotExist -> notFound
                BR.ETagMismatch (ETagged eTag (OwnedBy _ build)) ->
                    ProxyResponse
                        preconditionFailed412
                        (fromList [("ETag", hashToETagValue eTag)])
                        mempty
                        (applicationJson build)
    in fmap (either handler id) . runError

httpBuildServiceErrorHandler ::
       Sem ((Error BS.BuildServiceError) ': r) ProxyResponse
    -> Sem r ProxyResponse
httpBuildServiceErrorHandler =
    let handler =
            \case
                BS.StaticValidationError errors ->
                    ProxyResponse
                        badRequest400
                        mempty
                        mempty
                        -- TODO: Ideally these are formatted so they're more
                        -- actionable (both Create and Update)
                        (textPlain $ pack $ show errors)
                BS.ETagMismatch (ETagged eTag (OwnedBy _ build)) ->
                    ProxyResponse
                        preconditionFailed412
                        (fromList [("ETag", hashToETagValue eTag)])
                        mempty
                        (applicationJson build)
                BS.IllegalChange errors ->
                    ProxyResponse
                        conflict409
                        mempty
                        mempty
                        -- TODO: format these so their actionable
                        (textPlain $ pack $ show errors)
    in fmap (either handler id) . runError

invalidJson :: String -> ProxyResponse
invalidJson err = ProxyResponse badRequest400 mempty mempty (textPlain (pack err))

unauthorized :: ProxyResponse
unauthorized =
    ProxyResponse unauthorized401 mempty mempty (textPlain "Unauthorized")

badRequest :: ProxyResponse
badRequest = ProxyResponse badRequest400 mempty mempty (textPlain "Bad Request")

notImplemented :: ProxyResponse
notImplemented =
    ProxyResponse
        notImplemented501
        mempty
        mempty
        (textPlain "Not Yet Implemented")

notFound :: ProxyResponse
notFound = ProxyResponse notFound404 mempty mempty (textPlain "Not Found")

methodNotAllowed :: ProxyResponse
methodNotAllowed =
    ProxyResponse
        methodNotAllowed405
        mempty
        mempty
        (textPlain "Method Not Allowed")

forbidden :: ProxyResponse
forbidden = ProxyResponse forbidden403 mempty mempty (textPlain "Forbidden")

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
    -- /resources/users/${Text}
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

fromRight (Right x) = x
fromRight (Left x) = error "Attempted to fromRight a Left!"
