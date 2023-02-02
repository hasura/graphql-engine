{-# LANGUAGE ConstraintKinds #-}

-- | This module contains convenient wrapper functions to talk to the agent API from
-- inside tests. These wrapper functions automatically inject the 'API.SourceName'
-- and 'API.Config' into requests that require those. The `API.SourceName` comes
-- from the agent test context (see 'HasAgentTestContext' and
-- 'introduceAgentTestContext'). The 'API.Config' comes from either the agent
-- test context or the dataset context (see 'HasDatasetContext' and 'useDataset').
-- In practice, this means these API calls will either be talking to a dataset
-- clone database, if the agent supports datasets, or the database specified
-- on the command line by the user if the agent does not support datasets.
module Test.AgentAPI
  ( getCapabilitiesGuarded,
    guardCapabilitiesResponse,
    getHealth,
    getSourceHealth,
    getSchemaGuarded,
    guardSchemaResponse,
    queryGuarded,
    queryExpectError,
    explain,
    getMetrics,
    getSourceNameAndConfig,
    mergeAgentConfig,
  )
where

import Command (AgentConfig (..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
import Data.Text (Text)
import Hasura.Backends.DataConnector.API qualified as API
import Servant.API (NoContent, Union)
import Servant.Client ((//))
import Servant.Client.Generic (genericClient)
import Test.AgentClient (AgentClientT)
import Test.AgentDatasets (DatasetCloneInfo (..), DatasetContext (..), HasDatasetContext, getDatasetContext)
import Test.AgentTestContext (AgentTestContext (..), HasAgentTestContext, getAgentTestContext)
import Test.Sandwich (HasBaseContext, expectationFailure)

getCapabilitiesGuarded :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m API.CapabilitiesResponse
getCapabilitiesGuarded = guardCapabilitiesResponse =<< (genericClient // API._capabilities)

guardCapabilitiesResponse :: MonadThrow m => Union API.CapabilitiesResponses -> m API.CapabilitiesResponse
guardCapabilitiesResponse = API.capabilitiesCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected CapabilitiesResponse"
    successAction c = pure c
    errorAction e = expectationFailure $ "Expected CapabilitiesResponse, got " <> show e

getHealth :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m NoContent
getHealth = (genericClient // API._health) Nothing Nothing

getSourceHealth :: (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m NoContent
getSourceHealth = do
  (sourceName, config) <- getSourceNameAndConfig
  (genericClient // API._health) (Just sourceName) (Just config)

getSchemaGuarded :: (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m API.SchemaResponse
getSchemaGuarded = do
  (sourceName, config) <- getSourceNameAndConfig
  guardSchemaResponse =<< (genericClient // API._schema) sourceName config

guardSchemaResponse :: MonadThrow m => Union API.SchemaResponses -> m API.SchemaResponse
guardSchemaResponse = API.schemaCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected SchemaResponse"
    successAction c = pure c
    errorAction e = expectationFailure $ "Expected SchemaResponse, got " <> show e

queryGuarded :: (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.QueryRequest -> AgentClientT m API.QueryResponse
queryGuarded queryRequest = do
  (sourceName, config) <- getSourceNameAndConfig
  guardQueryResponse =<< (genericClient // API._query) sourceName config queryRequest

guardQueryResponse :: MonadThrow m => Union API.QueryResponses -> m API.QueryResponse
guardQueryResponse = API.queryCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected QueryResponse"
    successAction q = pure q
    errorAction e = expectationFailure $ "Expected QueryResponse, got " <> show e

queryExpectError :: (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.QueryRequest -> AgentClientT m API.ErrorResponse
queryExpectError queryRequest = do
  (sourceName, config) <- getSourceNameAndConfig
  guardQueryErrorResponse =<< (genericClient // API._query) sourceName config queryRequest

guardQueryErrorResponse :: MonadThrow m => Union API.QueryResponses -> m API.ErrorResponse
guardQueryErrorResponse = API.queryCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected ErrorResponse"
    successAction q = expectationFailure $ "Expected ErrorResponse, got " <> show q
    errorAction e = pure e

explain :: (HasBaseContext context, HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.QueryRequest -> AgentClientT m API.ExplainResponse
explain queryRequest = do
  (sourceName, config) <- getSourceNameAndConfig
  (genericClient // API._explain) sourceName config queryRequest

getMetrics :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m Text
getMetrics = genericClient // API._metrics

-------------------------------------------------------------------------------

supportsDatasets :: API.CapabilitiesResponse -> Bool
supportsDatasets = isJust . API._cDatasets . API._crCapabilities

getSourceNameAndConfig :: (HasAgentTestContext context, HasDatasetContext context, MonadReader context m, MonadThrow m) => m (API.SourceName, API.Config)
getSourceNameAndConfig = do
  AgentTestContext {..} <- getAgentTestContext
  case _atcAgentConfig of
    ManualConfig config -> pure (_atcSourceName, config)
    DatasetConfig mergeConfig ->
      if supportsDatasets _atcCapabilitiesResponse
        then do
          cloneInfo <- _dcClone <$> getDatasetContext
          case cloneInfo of
            Just DatasetCloneInfo {..} ->
              let mergedConfig = mergeAgentConfig _dciAgentConfig mergeConfig
               in pure (_atcSourceName, mergedConfig)
            Nothing -> expectationFailure "Expected a dataset clone to have been created, because the agent supports datasets, but one wasn't"
        else expectationFailure "An agent configuration is required to be provided if the agent does not support datasets"

mergeAgentConfig :: API.Config -> Maybe API.Config -> API.Config
mergeAgentConfig (API.Config configA) mergeConfig =
  let configB = maybe mempty API.unConfig mergeConfig
   in API.Config $ KeyMap.union configA configB
