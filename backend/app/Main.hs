{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.Runtime (mRuntime)
import BuildController
       (httpAuthorizer, httpBuildServiceErrorHandler,
        httpDynamoBuildRepoErrorHandler, httpHandler,
        httpVersionMismatchHandler)
import BuildRepo (buildRepoToDynamo)
import BuildService (buildServiceFromBuildRepo)
import Control.Lens (set)
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

main :: IO ()
main = do
    tableName <- pack <$> getEnv "TABLE_NAME"
    lgr <- newLogger Debug stderr
    awsEnv <- AWS.newEnv Discover
    let env = set AWS.envLogger lgr awsEnv
    runResourceT $
        flip runReaderT env $
        mRuntime
            (runM .
             httpAuthorizer .
             runReader tableName .
             httpVersionMismatchHandler .
             httpDynamoBuildRepoErrorHandler .
             buildRepoToDynamo .
             httpBuildServiceErrorHandler .
             buildServiceFromBuildRepo . httpHandler)
