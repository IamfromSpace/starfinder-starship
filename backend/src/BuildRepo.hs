{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Catch (MonadCatch, MonadThrow(..), catch)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Trans.AWS (AWSConstraint, send)
import Control.Monad.Trans.Resource
       (MonadResource, ResourceT, liftResourceT)
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
import Starfinder.Starship.Build (Build(Build, name))
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)

data SaveNewError =
    AlreadyExists

data UpdateError
    = DoesNotExist
    | ETagMismatch (ETagged (OwnedBy (Build Text ReferencedWeapon Text)))

class HasTableName a where
    getTableName :: a -> Text

class Monad m =>
      BuildRepoMonad m where
    saveNewBuild ::
           OwnedBy (Build Text ReferencedWeapon Text)
        -> m (Either SaveNewError Int)
    updateBuild ::
           Int
        -> OwnedBy (Build Text ReferencedWeapon Text)
        -> m (Either UpdateError Int)
    getBuild ::
           Text
        -> Text
        -> m (Maybe (ETagged (OwnedBy (Build Text ReferencedWeapon Text))))
    getBuildsByOwner :: a -> Text -> m [Text]

newtype BuildRepoT r m a = BuildRepoT
    { runBuildRepoT :: m a
    } deriving (Functor, Applicative, Monad)

instance MonadTrans (BuildRepoT r) where
    lift = BuildRepoT

instance (MonadReader r m, Monad m) => MonadReader r (BuildRepoT r m) where
    ask = lift ask
    local f ma = lift $ local f (runBuildRepoT ma)

instance (MonadThrow m, Monad m) => MonadThrow (BuildRepoT r m) where
    throwM = lift . throwM

instance (MonadCatch m, Monad m) => MonadCatch (BuildRepoT r m) where
    catch ma f = lift $ catch (runBuildRepoT ma) (runBuildRepoT . f)

instance MonadIO m => MonadIO (BuildRepoT r m) where
    liftIO = lift . liftIO

instance MonadResource m => MonadResource (BuildRepoT r m) where
    liftResourceT = lift . liftResourceT

instance (AWSConstraint r m, HasTableName r, MonadReader r m) =>
         BuildRepoMonad (BuildRepoT r m) where
    saveNewBuild ownedBuild = do
        tableName <- getTableName <$> ask
        let eTag = hash ownedBuild
        let item =
                set piExpressionAttributeNames (fromList
                   [ ("#hash", "HASH1")
                   , ("#range", "RANGE1.1")
                   ]) $
                set piConditionExpression (Just "attribute_not_exists(#hash) AND attribute_not_exists(#range)") $
                set
                    piItem
                    (ownedReferencedBuildToItem (ETagged eTag ownedBuild))
                    (putItem tableName)
        res <- trying _ConditionalCheckFailedException (send item)
        return $ bimap (const AlreadyExists) (const eTag) res
    updateBuild expectedETag ownedBuild@(OwnedBy userId (Build {name})) = do
        tableName <- getTableName <$> ask
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
        res <- trying _ConditionalCheckFailedException (send item)
        case res of
            Left _
              -- Since get is a consistent read, and ETags should be
              -- unguessable, there should be no chance that the ETag _now_
              -- matches if it didn't on write.  I think.
              -- TODO: A hashed salt helps with unguessability (but seems
              -- overkill).
             ->
                fmap
                    (Left . maybe DoesNotExist ETagMismatch)
                    (getBuild userId name)
            Right _ -> return $ Right newETag
    getBuild userId name = do
        tableName <- getTableName <$> ask
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
        res <- send item
        return $ fromAttrValue $ toAttrValue $ view girsItem res
    getBuildsByOwner = undefined
