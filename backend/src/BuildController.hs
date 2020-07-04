{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO: I'm not sure why this gets rid of my warning...
{-# LANGUAGE MonoLocalBinds #-}

module BuildController where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
       (ProxyRequest(..), authorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
       (ProxyResponse(..), applicationJson, badRequest400, conflict409,
        forbidden403, methodNotAllowed405, notFound404, notImplemented501,
        ok200, preconditionFailed412, textPlain, unauthorized401)
import BuildService
       (BuildServiceMonad(..), CreateError(..), GetError(..),
        UpdateError(..))
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap, fromList, lookup)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Lib (ETagged(..), OwnedBy(..))
import Network.HTTP.Types.URI (decodePathSegments)
import Prelude hiding (lookup)
import Starfinder.Starship.Build (Build)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)
import Text.Read (readMaybe)

-- TODO: These all seem to form a pattern where it's really just transforming
-- the Service result into ProxyResponse.  I'm still not totally convinced
-- that's the abstraction though.
handleCreate ::
       BuildServiceMonad Text m
    => Text
    -> Text
    -> Build Text ReferencedWeapon Text
    -> m ProxyResponse
handleCreate principal userId build = do
    errorOrETag <- saveNewBuild principal $ OwnedBy userId build
    case errorOrETag of
        Right eTag ->
            return
                (ProxyResponse
                     ok200
                     (fromList [("ETag", hashToETagValue eTag)])
                     mempty
                     (textPlain "Done"))
        Left NotAllowedC -> return forbidden
        Left AlreadyExists ->
            return
                (ProxyResponse
                     conflict409
                     mempty
                     mempty
                     (textPlain "User already has a ship with that name."))
        Left (StaticValidationErrorC errors) ->
            return
                (ProxyResponse
                     badRequest400
                     mempty
                     mempty
                     -- TODO: Ideally these are formatted so they're
                     -- more actionable (both Create and Update)
                     (textPlain $ pack $ show errors))

handleGet :: BuildServiceMonad Text m => Text -> Text -> Text -> m ProxyResponse
handleGet principal userId name = do
    record <- getBuild principal userId name
    case record of
        Right (ETagged eTag (OwnedBy _ build)) ->
            return
                (ProxyResponse
                     ok200
                     (fromList [("ETag", hashToETagValue eTag)])
                     mempty
                     (applicationJson build))
        Left DoesNotExistG -> return notFound
        Left NotAllowedG -> return forbidden

handlePut ::
       BuildServiceMonad Text m
    => Text
    -> Text
    -> Text
    -> Int
    -> Build Text ReferencedWeapon Text
    -> m ProxyResponse
handlePut principal userId name expectedETag build = do
    res <-
        updateBuild
            principal
            userId
            name
            expectedETag
            (const $ pure $ OwnedBy userId build)
    case res of
        Right eTag ->
            return
                (ProxyResponse
                     ok200
                     (fromList [("ETag", hashToETagValue eTag)])
                     mempty
                     (textPlain "Done"))
        Left DoesNotExistU -> return notFound
        Left (StaticValidationErrorU x) ->
            return
                (ProxyResponse
                     badRequest400
                     mempty
                     mempty
                     (textPlain (pack $ show x)))
        Left (ETagMismatch (ETagged eTag (OwnedBy _ build))) ->
            return
                (ProxyResponse
                     preconditionFailed412
                     (fromList [("ETag", hashToETagValue eTag)])
                     mempty
                     (applicationJson build))
        Left NotAllowedU -> return forbidden
        Left (IllegalChange x) ->
            return
                (ProxyResponse
                     conflict409
                     mempty
                     mempty
                     (textPlain $ pack $ show x))
        Left TransformError -> undefined -- impossible

httpHandler :: BuildServiceMonad Text m => ProxyRequest Text -> m ProxyResponse
httpHandler pr@(ProxyRequest {path, requestContext, body, httpMethod, headers}) =
    case (parseApiGatweayPath path, authorizer requestContext) of
        (_, Nothing) -> return unauthorized
        (Just (UserBuilds userId), Just principal) ->
            case httpMethod of
                "POST" ->
                    case decode body of
                        Just (build :: Build Text ReferencedWeapon Text) ->
                            handleCreate principal userId build
                        Nothing -> return badRequest
                _ -> return methodNotAllowed
        (Just (UserBuild userId name), Just principal) ->
            case httpMethod of
                "GET" -> handleGet principal userId name
                "PUT" ->
                    case (decode body, getAndParseETag headers) of
                        (Just (build :: Build Text ReferencedWeapon Text), Right expectedETag) ->
                            handlePut principal userId name expectedETag build
                        (Just (build :: Build Text ReferencedWeapon Text), Left NotPresent) ->
                            handleCreate principal userId build
                        (_, Left Invalid) ->
                            return badRequest -- clearly didn't get this from us, so they're doing something wrong.
                "PATCH" -> return notImplemented
                _ -> return methodNotAllowed
        (_, Just _) -> return notFound

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

parseApiGatweayPath :: Text -> Maybe (ValidPath)
parseApiGatweayPath =
    let parseSegments =
            \case
                ["resources", "users", u] -> Just $ UserBuilds u
                ["resources", "users", u, ""] -> Just $ UserBuilds u
                ["resources", "users", u, "builds", b] -> Just $ UserBuild u b
                ["resources", "users", u, "builds", b, ""] ->
                    Just $ UserBuild u b
                _ -> Nothing
    in parseSegments . filter (/= "") . decodePathSegments . encodeUtf8
