{-# LANGUAGE OverloadedStrings #-}

module Main where

import AWS.Lambda.Context (HasLambdaContext(..), LambdaContext)
import AWS.Lambda.Events.ApiGateway.ProxyRequest
       (authorizer, requestContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import BuildAuthorizer (authorizeUser)
import BuildController (getUserId, httpHandler)
import BuildRepo (buildRepoToDynamo)
import BuildService (buildServiceFromBuildRepo)
import Control.Lens (lens, set)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Network.AWS as AWS
import Network.AWS
       (Credentials(Discover), LogLevel(Debug), newLogger, runResourceT)
import Polysemy (runM)

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
            (\e ->
                 runM $
                 runReader
                 -- TODO: This is the only thing I don't like about this, where
                 -- otherwise abstraction boundaries are fairly cleanly
                 -- enforced.  Each piece only knows about the pieces it needs
                 -- to know about.  However, this doesn't seem to be part of
                 -- _setting up the application_, which I would expect to see
                 -- here.  Instead this seems to be doing the Controller's job
                 -- of extracting and translating HTTP data into domain
                 -- concepts.
                 --
                 -- The thing that makes this weird, is that if the controller
                 -- is going to send in the authorizer, then it _knows_ that an
                 -- authorizer is a requirement for a service, but with effects,
                 -- there's nothing that says that it _must_ be interpretted
                 -- with one.
                     (getUserId (fromJust (authorizer (requestContext e)))) $
                 authorizeUser $
                 runReader tableName $
                 buildRepoToDynamo $ buildServiceFromBuildRepo $ httpHandler e)
