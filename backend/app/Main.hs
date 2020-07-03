{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.Context (HasLambdaContext(..), LambdaContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import BuildController (httpHandler)
import BuildRepo (DynamoDbBuildRepo(..), HasTableName(..))
import BuildService (DefaultBuildService(..))
import Control.Lens (lens, set)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text, pack)
import qualified Network.AWS as AWS
import Network.AWS
       (Credentials(Discover), LogLevel(Debug), newLogger, runResourceT)
import System.Environment (getEnv)
import System.IO (stderr)

data Env = Env
    { awsEnv :: AWS.Env
    , tableName :: Text
    , lambdaContext :: Maybe LambdaContext
    }

instance AWS.HasEnv Env where
    environment = lens awsEnv (\env ae -> env {awsEnv = ae})

instance HasTableName Env where
    getTableName = tableName

instance HasLambdaContext Env where
    withContext lc env = env {lambdaContext = Just lc}

main :: IO ()
main = do
    tableName <- pack <$> getEnv "TABLE_NAME"
    lgr <- newLogger Debug stderr
    awsEnv <- AWS.newEnv Discover
    let env = Env (set AWS.envLogger lgr awsEnv) tableName Nothing
    let app =
            mRuntimeWithContext $
            httpHandler (DefaultBuildService DynamoDbBuildRepo)
    runResourceT $ runReaderT app env
