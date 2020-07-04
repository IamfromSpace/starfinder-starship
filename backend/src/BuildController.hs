{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import BuildService (BuildServiceMonad(..), UpdateError(..))
import Control.Monad ((>=>))
import Data.Aeson (decode)
import Data.CaseInsensitive (mk)
import Data.HashMap.Strict (fromList, lookup)
import Data.Text
import Lib (ETagged(..), OwnedBy(..))
import Prelude hiding (lookup)
import Starfinder.Starship.Build (Build)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)
import Text.Read (readMaybe)

httpHandler :: BuildServiceMonad Text m => ProxyRequest Text -> m ProxyResponse
httpHandler ProxyRequest {requestContext, body, httpMethod = "POST"} =
    case (decode body, authorizer requestContext) of
        (Just (build :: Build Text ReferencedWeapon Text), Just userId)
        -- TODO: Get the (target) userId from the url (and the build name with
        -- only PUT?)
         -> do
            errorOrETag <- saveNewBuild userId $ OwnedBy userId build
            case errorOrETag of
                Right eTag ->
                    return
                        (ProxyResponse
                             ok200
                             (fromList [("ETag", hashToETagValue eTag)])
                             mempty
                             (textPlain "Done"))
        -- TODO: Provide the errors
        -- TODO: AlreadyExists is a 409
        -- TODO: NotAllowed is a 403
                Left errors ->
                    return
                        (ProxyResponse
                             badRequest400
                             mempty
                             mempty
                             (textPlain $ pack $ show errors))
        (_, Nothing) ->
            return
                (ProxyResponse
                     unauthorized401
                     mempty
                     mempty
                     (textPlain "Unauthorized"))
        (Nothing, Just _) ->
            return
                (ProxyResponse
                     badRequest400
                     mempty
                     mempty
                     (textPlain "Bad Request"))
httpHandler ProxyRequest {requestContext, httpMethod = "GET"} =
    case authorizer requestContext of
        Just userId
            -- TODO: Dynamic Name and userId in path
         -> do
            record <- getBuild userId "Sunrise Maiden"
            case record of
                Just (ETagged eTag (OwnedBy _ build)) ->
                    return
                        (ProxyResponse
                             ok200
                             (fromList [("ETag", hashToETagValue eTag)])
                             mempty
                             (applicationJson build))
                Nothing ->
                    return
                        (ProxyResponse
                             notFound404
                             mempty
                             mempty
                             (textPlain "Not Found"))
        Nothing ->
            return
                (ProxyResponse
                     unauthorized401
                     mempty
                     mempty
                     (textPlain "Unauthorized"))
httpHandler ProxyRequest {requestContext, body, httpMethod = "PUT", headers} =
    let mExpectedETag = lookup (mk "If-Match") headers >>= eTagValueToHash
    in case (decode body, mExpectedETag, authorizer requestContext) of
           (Just (build :: Build Text ReferencedWeapon Text), Just expectedETag, Just userId)
            -- TODO: Dynamic Name and userId in path
            -> do
               res <-
                   updateBuild
                       userId
                       userId
                       "Sunrise Maiden"
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
                   Left DoesNotExist ->
                       return
                           (ProxyResponse
                                notFound404
                                mempty
                                mempty
                                (textPlain "Not Found"))
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
                   Left NotAllowedU ->
                       return
                           (ProxyResponse
                                forbidden403
                                mempty
                                mempty
                                (textPlain "Forbidden"))
                   Left (IllegalChange x) ->
                       return
                           (ProxyResponse
                                conflict409
                                mempty
                                mempty
                                (textPlain $ pack $ show x))
                   Left TransformError
                -- impossible
                    -> undefined
           (_, _, Nothing) ->
               return
                   (ProxyResponse
                        unauthorized401
                        mempty
                        mempty
                        (textPlain "Unauthorized"))
           (_, _, Just _) ->
               return
                   (ProxyResponse
                        badRequest400
                        mempty
                        mempty
                        (textPlain "Bad Request"))
httpHandler ProxyRequest {httpMethod = "PATCH"} =
    return
        (ProxyResponse
             notImplemented501
             mempty
             mempty
             (textPlain "Not Yet Implemented"))
httpHandler _ =
    return
        (ProxyResponse
             methodNotAllowed405
             mempty
             mempty
             (textPlain "Method Not Allowed"))

hashToETagValue :: Int -> Text
hashToETagValue = pack . show . show

eTagValueToHash :: Text -> Maybe Int
eTagValueToHash = readMaybe . unpack >=> readMaybe
