{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
module BuildService where

import qualified BuildRepo as BR
import Control.Exception.Lens (trying)
import Control.Lens (set, view)
import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader, ask)
import Data.Bifunctor (bimap)
import Data.Bifunctor (first)
import Data.HashMap.Strict (fromList)
import qualified Data.KeyedSet as KS
import Data.Text
import Lib
import Starfinder.Starship.Assets.Frames (frames)
import qualified Starfinder.Starship.Assets.Shields as S
import Starfinder.Starship.Assets.Weapons (weapons)
import Starfinder.Starship.Build
       (Build(..), BuildError, traverseFrame, traverseShields,
        traverseWeapon, validateStarship)

data CreateError
    = AlreadyExists
    -- TODO: figure out how to do duplicate names better
    -- This could maybe break into a couple subcategories:
    -- Permisions Errors and Static State errors
    | NotAllowedC
    | InvalidFrameC
    | InvalidWeaponC
    | InvalidShieldsC
    | BuildErrorC [BuildError]

data UpdateError
    = DoesNotExist
    | InvalidFrameU
    | InvalidWeaponU
    | InvalidShieldU
    | ETagMismatch
    | NotAllowedU
    | IllegalState [BuildError]
    | BuildErrorU
    | IllegalFrameChange

class BuildService m u a where
    saveNewBuild ::
           a
        -> u
        -> OwnedBy (Build Text Text Text)
        -> m (Either [CreateError] Text)
    updateBuild ::
           a
        -> u
        -> Text
        -> OwnedBy (Build Text Text Text)
        -> m (Either [UpdateError] Text)
    getBuild ::
           a
        -> u
        -> Text
        -> m (Maybe (ETagged (OwnedBy (Build Text Text Text))))
    getBuildsByOwner :: u -> a -> Text -> m [Text]

data DefaultBuildService r =
    DefaultBuildService r

instance (Monad m, BR.BuildRepo m r) =>
         BuildService m Text (DefaultBuildService r) where
    saveNewBuild (DefaultBuildService r) principal v@(OwnedBy userId build) =
        let findOrError e ks = maybe (Left [e]) Right . flip KS.lookup ks
            authorize x =
                if principal /= userId
                    then Left [NotAllowedC]
                    else Right x
            populateFrame = traverseFrame (findOrError InvalidFrameC frames)
            populateWeapons =
                traverseWeapon (findOrError InvalidWeaponC weapons)
            populateShields =
                traverseShields (findOrError InvalidShieldsC S.shields)
            validateBuild =
                first (pure . BuildErrorC) .
                emptyWithSelfError . validateStarship
            authorizePopulateAndValidate =
                authorize >=>
                populateFrame >=>
                populateWeapons >=> populateShields >=> validateBuild
            mapSaveBuildError =
                (first
                     (\case
                          BR.AlreadyExists -> [AlreadyExists]))
        in case authorizePopulateAndValidate build of
               Left es -> return $ Left es
               Right _ -> mapSaveBuildError <$> BR.saveNewBuild r v
    updateBuild = undefined
    getBuild (DefaultBuildService r) = BR.getBuild r
    getBuildsByOwner = undefined

emptyWithSelfError :: [a] -> Either [a] ()
emptyWithSelfError [] = Right ()
emptyWithSelfError xs = Left xs
