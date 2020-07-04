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
import Data.Maybe (catMaybes)
import Data.Text
import Lib
import Starfinder.Starship.Assets.Frames (frames)
import qualified Starfinder.Starship.Assets.Shields as S
import Starfinder.Starship.Assets.Weapons (dereferenceWeapon)
import Starfinder.Starship.Build
       (Build(..), BuildError, traverseFrame, traverseShields,
        traverseWeapon, validateStarship)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)

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

data CreateError
    = AlreadyExists
    -- TODO: figure out how to do duplicate names better
    -- This could maybe break into a couple subcategories:
    -- Permisions Errors and Static State errors
    | NotAllowedC
    | StaticValidationErrorC [StaticValidationError]
    deriving (Show)

data UpdateError
    = DoesNotExistU
    | StaticValidationErrorU [StaticValidationError]
    -- TODO: We don't really want this to be an ETag in any context except the
    -- controller context
    | ETagMismatch (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
    | NotAllowedU
    | IllegalChange [ChangeValidationError]
    -- TODO: Ideally this is paramaterized
    | TransformError
    deriving (Show)

data GetError
    = NotAllowedG
    | DoesNotExistG
    deriving (Show)

class Monad m =>
      BuildServiceMonad u m where
    saveNewBuild ::
           u
        -> OwnedBy (Build Text ReferencedWeapon Text)
        -> m (Either CreateError Int)
    updateBuild ::
           u
        -> Text
        -> Text
        -> Int
        -- TODO: Ideally this could fail with a specific error or even happen in
        -- its own monad!
        -> (OwnedBy (Build Text ReferencedWeapon Text) -> Maybe (OwnedBy (Build Text ReferencedWeapon Text)))
        -> m (Either UpdateError Int)
    getBuild ::
           u
        -> Text
        -> Text
        -> m (Either GetError (ETagged (OwnedBy (Build Text ReferencedWeapon Text))))
    getBuildsByOwner :: u -> Text -> m [Text]

-- TODO: I think that in theory this also needs a BuildServiceT to get rid of
-- some of the warnings around type inference woes, however, that's yet more
-- boilerplate involved!
instance BR.BuildRepoMonad m => BuildServiceMonad Text m where
    saveNewBuild principal v@(OwnedBy userId build) =
        let authorize x =
                if principal /= userId
                    then Left NotAllowedC
                    else Right x
            authorizePopulateAndValidate =
                authorize >=>
                ((first StaticValidationErrorC) . populateAndValidate)
            mapSaveBuildError =
                (first
                     (\case
                          BR.AlreadyExists -> AlreadyExists))
        in case authorizePopulateAndValidate build of
               Left es -> return $ Left es
               Right _ -> mapSaveBuildError <$> BR.saveNewBuild v
    updateBuild principal userId name expectedETag f =
        if principal /= userId
            then return $ Left NotAllowedU
            else do
                getRes <- getBuild principal userId name
                let mapGetError =
                        \case
                            NotAllowedG -> NotAllowedU
                            DoesNotExistG -> DoesNotExistU
                let eNewOwnedBuild = do
                        (ceob@(ETagged currentETag currentOwnedBuild)) <-
                            first mapGetError getRes
                        if currentETag /= expectedETag
                            then Left $ ETagMismatch ceob
                            else Right ()
                        (newOwnedBuild@(OwnedBy _ newBuild)) <-
                            case f currentOwnedBuild of
                                Nothing -> Left TransformError
                                Just x -> Right x
                        first StaticValidationErrorU $
                            populateAndValidate newBuild
                        case validateChange currentOwnedBuild newOwnedBuild of
                            [] -> return newOwnedBuild
                            es -> Left $ IllegalChange es
                let mapBuildRepoError =
                        \case
                            BR.DoesNotExist -> DoesNotExistU
                            BR.ETagMismatch x -> ETagMismatch x
                let withNewOwnedBuild newOwnedBuild =
                        let putRes = BR.updateBuild expectedETag newOwnedBuild
                        in first mapBuildRepoError <$> putRes
                either (return . Left) withNewOwnedBuild eNewOwnedBuild
    getBuild principal userId name =
        if principal /= userId
            then return $ Left NotAllowedG
            else maybe (Left DoesNotExistG) Right <$> BR.getBuild userId name
    getBuildsByOwner = undefined

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
            traverseWeapon (orError InvalidShields . dereferenceWeapon)
        populateShields = traverseShields (findOrError InvalidShields S.shields)
        validateBuild =
            first (fmap BuildError) . emptyWithSelfError . validateStarship
    in populateFrame >=>
       populateWeapons >=> populateShields >=> validateBuild >=> const (pure ())

emptyWithSelfError :: [a] -> Either [a] ()
emptyWithSelfError [] = Right ()
emptyWithSelfError xs = Left xs
