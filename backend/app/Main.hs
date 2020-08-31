{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.Context (HasLambdaContext(..), LambdaContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import BuildController
       (httpAuthorizer, httpBuildServiceErrorHandler,
        httpDynamoBuildRepoErrorHandler, httpHandler,
        httpVersionMismatchHandler)
import BuildRepo (buildRepoToDynamo)
import BuildService (buildServiceFromBuildRepo)
import Control.Lens (lens, set)
import Control.Monad.Reader (runReaderT)
import Data.Text (pack)
import qualified Network.AWS as AWS
import Network.AWS
       (Credentials(Discover), LogLevel(Debug), newLogger, runResourceT)

-- This is here to enable optimizations
import Polysemy.Internal
import Polysemy.Reader (runReader)
import System.Environment (getEnv)
import System.IO (stderr)

data Env = Env
    { awsEnv :: AWS.Env
    , lambdaContext :: Maybe LambdaContext
    }

instance AWS.HasEnv Env where
    environment = lens awsEnv (\env ae -> env {awsEnv = ae})

instance HasLambdaContext Env where
    withContext lc env = env {lambdaContext = Just lc}

main :: IO ()
main = do
    tableName <- pack <$> getEnv "TABLE_NAME"
    lgr <- newLogger Debug stderr
    awsEnv <- AWS.newEnv Discover
    let env = Env (set AWS.envLogger lgr awsEnv) Nothing
    runResourceT $
        flip runReaderT env $
        mRuntimeWithContext
            (runM .
             httpAuthorizer .
             runReader tableName .
             httpVersionMismatchHandler .
             httpDynamoBuildRepoErrorHandler .
             buildRepoToDynamo .
             httpBuildServiceErrorHandler .
             buildServiceFromBuildRepo . httpHandler)
