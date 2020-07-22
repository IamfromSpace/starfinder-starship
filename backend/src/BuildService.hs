{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Authorizer (Authorizer, checkActionAuthorized)
import qualified BuildRepo as BR
import Control.Exception.Lens (trying)
import Control.Lens (set, view)
import Control.Monad ((>=>), when)
import Control.Monad.Reader (MonadReader, ask)
import Data.Bifunctor (bimap)
import Data.Bifunctor (first)
import Data.HashMap.Strict (fromList)
import qualified Data.KeyedSet as KS
import Data.Maybe (catMaybes)
import Data.Text
import Lib
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, fromEither, throw)
import Starfinder.Starship.Assets.Frames (frames)
import qualified Starfinder.Starship.Assets.Shields as S
import Starfinder.Starship.Assets.Weapons (dereferenceWeapon)
import Starfinder.Starship.Build
       (Build(..), BuildError, traverseFrame, traverseShields,
        traverseWeapon, validateStarship)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)

data Action
    = CreateStarshipBuild Text -- userId
    | UpdateStarshipBuild Text
                          Text --userId name
    | GetStarshipBuild Text
                       Text -- userId name

data StaticValidationError
    = InvalidFrame
    | InvalidWeapon
    | InvalidShields
    | BuildError BuildError
    deriving (Show)

data ChangeValidationError
    = UserChanged
    | NameChanged
    | FrameChanged
    deriving (Show)

data BuildServiceError
    = StaticValidationError [StaticValidationError]
    -- TODO: We don't really want this to be an ETag in any context except the
    -- controller context
    -- TODO: This should be pulled out as a common error
    | ETagMismatch (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
    | IllegalChange [ChangeValidationError]
    deriving (Show)

data BuildService x m a where
    SaveNewBuild
        :: OwnedBy (Build Text ReferencedWeapon Text) -> BuildService x m Int
    UpdateBuild
        :: Text
        -> Text
        -> Int
        -- TODO: Ideally this could fail with a specific error or even happen in
        -- its own monad!
        -> (OwnedBy (Build Text ReferencedWeapon Text) -> Either x (OwnedBy (Build Text ReferencedWeapon Text)))
        -> BuildService x m (Either x Int)
    GetBuild
        :: Text
        -> Text
        -> BuildService x m (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
    GetBuildsByOwner :: Text -> BuildService x m [Text]

makeSem ''BuildService

buildServiceFromBuildRepo ::
       ( Member BR.BuildRepo r
       , Member (Authorizer Action) r
       , Member (Error BuildServiceError) r
       )
    => Sem (BuildService x ': r) a
    -> Sem r a
buildServiceFromBuildRepo =
    interpret $ \case
        SaveNewBuild v@(OwnedBy userId build) -> do
            checkActionAuthorized (CreateStarshipBuild userId)
            fromEither $ first StaticValidationError $ populateAndValidate build
            BR.saveNewBuild v
        UpdateBuild userId name expectedETag f -> do
            checkActionAuthorized (UpdateStarshipBuild userId name)
            (ceob@(ETagged currentETag currentOwnedBuild)) <-
                getBuild' userId name
            when (currentETag /= expectedETag) $ throw $ ETagMismatch ceob
            let withNew (newOwnedBuild@(OwnedBy _ newBuild)) = do
                    fromEither $
                        first (StaticValidationError) $
                        populateAndValidate newBuild
                    case validateChange currentOwnedBuild newOwnedBuild of
                        [] -> return newOwnedBuild
                        es -> throw $ IllegalChange es
                    BR.updateBuild expectedETag newOwnedBuild
            traverse withNew $ f currentOwnedBuild
        GetBuild userId name -> getBuild' userId name
        GetBuildsByOwner _ -> undefined

getBuild' ::
       Member BR.BuildRepo r
    => Member (Authorizer Action) r =>
           Text -> Text -> Sem r (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
getBuild' userId name = do
    checkActionAuthorized (GetStarshipBuild userId name)
    BR.getBuild userId name

validateChange ::
       OwnedBy (Build Text a b)
    -> OwnedBy (Build Text a b)
    -> [ChangeValidationError]
validateChange a b =
    let OwnedBy userIdA Build {frame = frameA, name = nameA} = a
        OwnedBy userIdB Build {frame = frameB, name = nameB} = b
    in catMaybes
           [ if userIdA /= userIdB
                 then Just UserChanged
                 else Nothing
           , if nameA /= nameB
                 then Just NameChanged
                 else Nothing
           , if frameA /= frameB
                 then Just FrameChanged
                 else Nothing
           ]

populateAndValidate ::
       Build Text ReferencedWeapon Text -> Either [StaticValidationError] ()
populateAndValidate =
    let orError e = maybe (Left [e]) Right
        findOrError e ks = orError e . flip KS.lookup ks
        populateFrame = traverseFrame (findOrError InvalidFrame frames)
        populateWeapons =
            traverseWeapon (orError InvalidWeapon . dereferenceWeapon)
        populateShields = traverseShields (findOrError InvalidShields S.shields)
        validateBuild =
            first (fmap BuildError) . emptyWithSelfError . validateStarship
    in populateFrame >=>
       populateWeapons >=> populateShields >=> validateBuild >=> const (pure ())

emptyWithSelfError :: [a] -> Either [a] ()
emptyWithSelfError [] = Right ()
emptyWithSelfError xs = Left xs
