{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : BuildService
Description : Operations for saving and retreiving Starship Builds
Copyright   : (c) Nathan Fairhurst, 2020
License     : MIT
Maintainer  : nathan.p3pictures@gmail.com
Stability   : experimental

This is a higher level module aimed at saving and retreiving Builds for users.
It validates both states and changes, in the context of the principal creating
the change.
-}
module BuildAuthorizer where

import Authorizer (Authorizer(..))
import BuildService (Action(..))
import Data.Text (Text)
import Polysemy (Member, Sem, interpret)
import Polysemy.Error (Error, throw)
import Polysemy.Reader (Reader, ask)

data AuthError
    = Unidentified
    | Forbidden

authorizeUser ::
       (Member (Reader (Maybe Text)) r, Member (Error AuthError) r)
    => Sem ((Authorizer Action) ': r) b
    -> Sem r b
authorizeUser =
    interpret $ \case
        CheckActionAuthorized (CreateStarshipBuild userId) ->
            ask >>=
            (\case
                 Nothing -> throw Unidentified
                 Just ((==) userId -> False) -> throw Forbidden
                 _ -> return ())
        CheckActionAuthorized (UpdateStarshipBuild userId _) ->
            ask >>=
            (\case
                 Nothing -> throw Unidentified
                 Just ((==) userId -> False) -> throw Forbidden
                 _ -> return ())
        CheckActionAuthorized (GetStarshipBuild userId _) ->
            ask >>=
            (\case
                 Nothing -> throw Unidentified
                 Just ((==) userId -> False) -> throw Forbidden
                 _ -> return ())
