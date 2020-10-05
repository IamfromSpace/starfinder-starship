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
import Control.Monad ((<=<))
import Control.Monad.Trans.AWS (AWSConstraint, send)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (fromList, lookup)
import Data.Hashable (Hashable(..))
import Data.Maybe (fromMaybe)
import Data.Text hiding (reverse)
import Error.VersionMismatch (VersionMismatch(..))
import Lib
import Network.AWS.DynamoDB.GetItem
       (getItem, giConsistentRead, giKey, girsItem)
import Network.AWS.DynamoDB.PutItem
       (piConditionExpression, piExpressionAttributeNames,
        piExpressionAttributeValues, piItem, putItem)
import Network.AWS.DynamoDB.Query
       (qExpressionAttributeNames, qExpressionAttributeValues,
        qKeyConditionExpression, qProjectionExpression, qrsItems, query)
import Network.AWS.DynamoDB.Types
       (_ConditionalCheckFailedException)
import Polysemy (makeSem, Sem, Member, Embed, interpret)
import Polysemy.Embed (embed)
import Polysemy.Error (Error, throw)
import Polysemy.Reader (Reader, ask)
import Polysemy.State (State, get, put)
import Prelude hiding (lookup)
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
    GetBuildsByOwner :: Text -> BuildRepo m [Text]

makeSem ''BuildRepo

data DynamoBuildRepoError
    = AlreadyExists
    | DoesNotExist

buildRepoToDynamo ::
       ( AWSConstraint ar m
       , Member (Reader Text) r
       , Member (Embed m) r
       , Member (Error DynamoBuildRepoError) r
       , Member (Error (VersionMismatch (OwnedBy (Build Text ReferencedWeapon Text)))) r
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
                 ->
                   throw =<< VersionMismatch <$> getBuild' userId name
                Right _ -> return newETag
        GetBuild userId name -> getBuild' userId name
        GetBuildsByOwner userId -> do
            tableName <- ask
            res <-
                embed $
                send $
                set
                    qExpressionAttributeValues
                    (fromList [(":thisUser", toAttrValue userId)]) $
                set
                    qExpressionAttributeNames
                    (fromList [("#userId", "HASH1"), ("#name", "RANGE1.1")]) $
                set qKeyConditionExpression (Just "#userId = :thisUser") $
                set qProjectionExpression (Just "#name") $ query tableName
            -- TODO: Our current pattern assumes that we're going to get all
            -- items, so we either need to do that, or we need to make our
            -- interface more complex (counts/last/etc)
            let items = view qrsItems res
            return $
                -- This should never happen, not sure we could do much about it
                fromMaybe (error "Couldn't get a name from starship build!") $
                traverse (fromAttrValue <=< lookup "RANGE1.1") items

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

type MockState = Map (Text, Text) (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))

mockBuildRepoToDynamo ::
       ( Member (Error DynamoBuildRepoError) r
       , Member (Error (VersionMismatch (OwnedBy (Build Text ReferencedWeapon Text)))) r
       , Member (State MockState) r
       )
    => Sem (BuildRepo ': r) a
    -> Sem r a
mockBuildRepoToDynamo =
    interpret $ \case
        SaveNewBuild ownedBuild@(OwnedBy owner Build { name }) -> do
            let eTag = hash ownedBuild
            current <- get
            case Map.lookup (owner, name) current of
                Just _  -> throw AlreadyExists
                Nothing -> do
                  let next = Map.insert (owner, name) (ETagged eTag ownedBuild) current
                  put next
                  return eTag
        UpdateBuild expectedETag ownedBuild@(OwnedBy owner Build { name }) -> do
            let eTag = hash ownedBuild
            current <- get
            case Map.lookup (owner, name) current of
                Nothing -> throw DoesNotExist
                Just e@(ETagged oldETag _) ->
                  if oldETag /= expectedETag then
                    throw $ VersionMismatch e
                  else do
                    let next = Map.insert (owner, name) (ETagged eTag ownedBuild) current
                    put next
                    return eTag
        GetBuild userId name -> do
            current <- get
            case Map.lookup (userId, name) current of
                Nothing  -> throw DoesNotExist
                Just x -> return x
        GetBuildsByOwner userId -> do
            current <- get
            -- Get all items >= our users Id
            let (_, emp, gt) = Map.splitLookup (userId, "") current
            -- Remove all items > our user's id
            let (eq, _, _) = Map.splitLookup (succUuid userId, "") gt
            -- Convert this to a list, and take only the names
            let names = snd <$> Map.keys eq
            -- If we got a match for the "", add it to the list
            let items = fromMaybe names (const ("" : names) <$> emp)
            return items

-- TODO: We should really use a UUID type!
-- TODO: Could just be succ on UUID type
-- TODO: Assumes lower case!
succUuid :: Text -> Text
succUuid t = pack $ reverse $ succUuid' $ reverse $ unpack t

succUuid' :: String -> String
-- We found the max uuid!  Need to maintain string length, rather than roll.
-- We should buy some lotto tickets...
succUuid' ['f'] = ['g']
succUuid' ('-':t) = '-' : succUuid' t
succUuid' ('f':t) = '0' : succUuid' t
-- 9 is a special case in character layout vs value
succUuid' ('9':t) = 'a' : t
-- Otherwise, succ will increment correctly
succUuid' (c:t) = succ c : t
succUuid' [] = []
