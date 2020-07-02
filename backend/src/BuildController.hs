{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildController where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
       (ProxyRequest(..), authorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
       (ProxyResponse(..), badRequest400, methodNotAllowed405,
        notImplemented501, ok200, textPlain, unauthorized401)
import BuildService (BuildService(..))
import Data.Aeson (decode)
import Data.Text
import Lib (OwnedBy(..))
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
        (Just _, Nothing) ->
            return
                (ProxyResponse
                     unauthorized401
                     mempty
                     mempty
                     (textPlain "Unauthorized"))
        (Nothing, _) ->
            return
                (ProxyResponse
                     badRequest400
                     mempty
                     mempty
                     (textPlain "Bad Request"))
httpHandler _ ProxyRequest {httpMethod = "GET"} =
    return
        (ProxyResponse
             notImplemented501
             mempty
             mempty
             (textPlain "Not Yet Implemented"))
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
