{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Lens (set)
import Control.Monad.Reader (MonadReader, ask)
import Data.Bifunctor (bimap)
import Data.Text
import Lib
import Network.AWS (MonadAWS, send)
import Network.AWS.DynamoDB.PutItem
       (PutItem, piConditionExpression, piItem, putItem)
import Network.AWS.DynamoDB.Types
       (_ConditionalCheckFailedException)
import Starfinder.Starship.Build (Build)

data SaveNewError =
    AlreadyExists

data UpdateError
    = DoesNotExist
    | ETagMismatch

class HasTableName a where
    getTableName :: a -> Text

class BuildRepo m a where
    saveNewBuild ::
           a -> OwnedBy (Build Text Text Text) -> m (Either SaveNewError Text)
    updateBuild ::
           a
        -> Text
        -> OwnedBy (Build Text Text Text)
        -> m (Either UpdateError Text)
    getBuild :: a -> Text -> Text -> m (OwnedBy (Build Text Text Text))
    getBuildsByOwner :: a -> Text -> m [Text]

-- TODO: r is a phantom type?  Do we need it?
data DynamoDbBuildRepo r =
    DynamoDbBuildRepo

instance (MonadAWS m, MonadReader r m, HasTableName r) =>
         BuildRepo m (DynamoDbBuildRepo r) where
    saveNewBuild _ ownedBuild = do
        tableName <- getTableName <$> ask
        -- TODO: Calculate ETag via Hashable Typeclass
        let item =
                set piConditionExpression (Just "attribute_not_exists") $
                set
                    piItem
                    (ownedReferencedBuildToItem (ETagged "\"TODO\"" ownedBuild))
                    (putItem tableName)
        res <- trying _ConditionalCheckFailedException (send item)
        return $ bimap (const AlreadyExists) (const "\"TODO\"") res
    updateBuild = undefined
    getBuild = undefined
    getBuildsByOwner = undefined
