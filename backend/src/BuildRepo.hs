{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : BuildRepo
Description : Operations for saving and retreiving Starship Builds
Copyright   : (c) Nathan Fairhurst, 2020
License     : MIT
Maintainer  : nathan.p3pictures@gmail.com
Stability   : experimental

This is a very low level module that makes absolutely no guarantees about the
quality of the data that it saves.  Validating items before they are saved
should happen at a higher level.

This exposes both a typeclass and the default DynamoDb based implementation.
All operations happen inside of your chosen monad, and it should encapsulate
all details beyond the ones of interest to the interface.
-}
module BuildRepo where


import Control.Exception.Lens (trying)
import Control.Lens (set, view)
import Control.Monad.Trans.AWS (AWSConstraint, send)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict (fromList)
import Data.Hashable (Hashable(..))
import Data.Text
import Lib
import Network.AWS.DynamoDB.GetItem
       (GetItem, getItem, giConsistentRead, giKey, girsItem)
import Network.AWS.DynamoDB.PutItem
       (PutItem, piConditionExpression, piExpressionAttributeNames,
        piExpressionAttributeValues, piItem, putItem)
import Network.AWS.DynamoDB.Types
       (_ConditionalCheckFailedException)
import Polysemy (makeSem, Sem, Member, Embed, interpret)
import Polysemy.Embed (embed)
import Polysemy.Error (Error, throw)
import Polysemy.Reader (Reader, ask)
import Starfinder.Starship.Build (Build(Build, name))
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)

data BuildRepo m a where
    SaveNewBuild
        :: OwnedBy (Build Text ReferencedWeapon Text) -> BuildRepo m Int
    UpdateBuild
        :: Int -> OwnedBy (Build Text ReferencedWeapon Text) -> BuildRepo m Int
    GetBuild
        :: Text
        -> Text
        -> BuildRepo m (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
    GetBuildsByOwner :: a -> Text -> BuildRepo m [Text]

makeSem ''BuildRepo

data DynamoBuildRepoError
    = AlreadyExists
    | DoesNotExist
    | ETagMismatch (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))

buildRepoToDynamo ::
       ( AWSConstraint ar m
       , Member (Reader Text) r
       , Member (Embed m) r
       , Member (Error DynamoBuildRepoError) r
       )
    => Sem (BuildRepo ': r) a
    -> Sem r a
buildRepoToDynamo =
    interpret $ \case
        SaveNewBuild ownedBuild -> do
            tableName <- ask
            let eTag = hash ownedBuild
            let item =
                    set
                        piExpressionAttributeNames
                        (fromList [("#hash", "HASH1"), ("#range", "RANGE1.1")]) $
                    set
                        piConditionExpression
                        (Just
                             "attribute_not_exists(#hash) AND attribute_not_exists(#range)") $
                    set
                        piItem
                        (ownedReferencedBuildToItem (ETagged eTag ownedBuild))
                        (putItem tableName)
            res <- embed $ trying _ConditionalCheckFailedException (send item)
            case res of
                Left _ -> throw AlreadyExists
                Right _ -> return eTag
        UpdateBuild expectedETag ownedBuild@(OwnedBy userId (Build {name})) -> do
            tableName <- ask
            let newETag = hash ownedBuild
            let item =
                    set
                        piExpressionAttributeValues
                        (fromList [(":eTag", toAttrValue expectedETag)]) $
                    set
                        piExpressionAttributeNames
                        (fromList
                             [ ("#eTag", "eTag")
                             , ("#hash", "HASH1")
                             , ("#range", "RANGE1.1")
                             ]) $
                    set
                        piConditionExpression
                        (Just
                             "#eTag = :eTag AND attribute_exists(#hash) AND attribute_exists(#range)") $
                    set
                        piItem
                        (ownedReferencedBuildToItem (ETagged newETag ownedBuild))
                        (putItem tableName)
            res <- embed $ trying _ConditionalCheckFailedException (send item)
            case res of
                Left _
              -- Since get is a consistent read, and ETags should be
              -- unguessable, there should be no chance that the ETag _now_
              -- matches if it didn't on write.  I think.
              -- TODO: A hashed salt helps with unguessability (but seems
              -- overkill).
                 -> throw =<< ETagMismatch <$> getBuild' userId name
                Right _ -> return newETag
        GetBuild userId name -> getBuild' userId name
        GetBuildsByOwner _ _ -> undefined

getBuild' ::
       ( AWSConstraint ar m
       , Member (Reader Text) r
       , Member (Embed m) r
       , Member (Error DynamoBuildRepoError) r
       )
    => Text
    -> Text
    -> Sem r (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))
getBuild' userId name = do
    tableName <- ask
    let item
            -- To keep our typeclass simple, we only support the safe
            -- approach of reading consistently
         =
            set giConsistentRead (Just True) $
            -- TODO: Another place where typed keys would help
            set
                giKey
                (fromList
                     [ ("HASH1", toAttrValue userId)
                     , ("RANGE1.1", toAttrValue name)
                     ])
                (getItem tableName)
    res <- embed $ send item
    maybe (throw DoesNotExist) return $
        fromAttrValue $ toAttrValue $ view girsItem res
