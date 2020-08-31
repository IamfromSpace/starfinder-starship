{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Maybe (fromJust)
import Data.Text (Text)
import Polysemy (Member, Sem, interpret)
import Polysemy.Error (Error, throw)
import Polysemy.Reader (Reader, ask)

data Forbidden =
    Forbidden

authorizeUser ::
       (Member (Reader (Maybe Text)) r, Member (Error Forbidden) r)
    => Sem ((Authorizer Action) ': r) b
    -> Sem r b
authorizeUser =
    interpret $ \case
        CheckActionAuthorized (CreateStarshipBuild userId') -> do
            -- TODO: Unauthorized on Nothing
            userId <- fromJust <$> ask
            if userId /= userId'
                then throw Forbidden
                else return ()
        CheckActionAuthorized (UpdateStarshipBuild userId' _) -> do
            userId <- fromJust <$> ask
            if userId /= userId'
                then throw Forbidden
                else return ()
        CheckActionAuthorized (GetStarshipBuild userId' _) -> do
            userId <- fromJust <$> ask
            if userId /= userId'
                then throw Forbidden
                else return ()
