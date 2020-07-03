{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildController where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
       (ProxyRequest(..), authorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
       (ProxyResponse(..), applicationJson, badRequest400,
        methodNotAllowed405, notImplemented501, ok200, textPlain,
        unauthorized401, notFound404)
import BuildService (BuildService(..))
import Data.Aeson (decode)
import Data.Text
import Lib (ETagged(..), OwnedBy(..))
import Starfinder.Starship.Build (Build)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)

httpHandler ::
       (Monad m, BuildService m Text a)
    => a
    -> ProxyRequest Text
    -> m ProxyResponse
httpHandler service ProxyRequest {requestContext, body, httpMethod = "POST"} =
    case (decode body, authorizer requestContext) of
        (Just (build :: Build Text ReferencedWeapon Text), Just userId)
        -- TODO: Get the (target) userId from the url (and the build name with
        -- only PUT?)
         -> do
            errorOrETag <- saveNewBuild service userId $ OwnedBy userId build
            case errorOrETag
        -- TODO: Provide the ETag
                  of
                Right eTag ->
                    return
                        (ProxyResponse ok200 mempty mempty (textPlain "Done"))
        -- TODO: Provide the errors
                Left errors ->
                    return
                        (ProxyResponse
                             badRequest400
                             mempty
                             mempty
                             (textPlain "Error"))
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
httpHandler service ProxyRequest {requestContext, httpMethod = "GET"} =
    case authorizer requestContext of
        Just userId
            -- TODO: Dynamic Name and userId in path
         -> do
            record <- getBuild service userId "Sunrise Maiden"
            case record of
                Just (ETagged eTag (OwnedBy _ build)) ->
                    return (ProxyResponse ok200 mempty mempty (applicationJson build))
                Nothing ->
                    return (ProxyResponse notFound404 mempty mempty (textPlain "Not Found"))
        Nothing ->
            return
                (ProxyResponse
                     unauthorized401
                     mempty
                     mempty
                     (textPlain "Unauthorized"))
httpHandler _ ProxyRequest {httpMethod = "PUT"} =
    return
        (ProxyResponse
             notImplemented501
             mempty
             mempty
             (textPlain "Not Yet Implemented"))
httpHandler _ ProxyRequest {httpMethod = "PATCH"} =
    return
        (ProxyResponse
             notImplemented501
             mempty
             mempty
             (textPlain "Not Yet Implemented"))
httpHandler _ _ =
    return
        (ProxyResponse
             methodNotAllowed405
             mempty
             mempty
             (textPlain "Method Not Allowed"))
