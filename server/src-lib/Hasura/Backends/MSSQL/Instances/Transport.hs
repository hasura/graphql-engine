{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Transport () where

import           Hasura.Prelude

import qualified Data.ByteString                         as B
import qualified Language.GraphQL.Draft.Syntax           as G

import           Data.Text.Encoding                      (encodeUtf8)
import           Data.Text.Extended
import           Hasura.RQL.Types.Error                  as HE

import qualified Hasura.Logging                          as L

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Instances.Execute
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Logging                  (MonadQueryLog (..))
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types
import           Hasura.Server.Types                     (RequestId)
import           Hasura.Session
import           Hasura.Tracing


instance BackendTransport 'MSSQL  where
  runDBQuery = runQuery
  runDBMutation = runMutation
  runDBSubscription = runSubscription

runQuery
  :: ( MonadIO m
     , MonadQueryLog m
     , MonadTrace m
     , MonadError QErr m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name
  -> UserInfo
  -> L.Logger L.Hasura
  -> SourceConfig 'MSSQL
  -> ExceptT QErr IO EncJSON
  -> Maybe Text
  -> m (DiffTime, EncJSON)
  -- ^ Also return the time spent in the PG query; for telemetry.
runQuery reqId query fieldName _userInfo logger _sourceConfig tx _genSql =  do
  -- log the generated SQL and the graphql query
  -- FIXME: fix logging by making logQueryLog expect something backend agnostic!
  logQueryLog logger query Nothing reqId
  withElapsedTime
    $ trace ("MSSQL Query for root field " <>> fieldName)
    $ run tx

runMutation
  :: ( MonadIO m
     , MonadQueryLog m
     , MonadTrace m
     , MonadError QErr m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name
  -> UserInfo
  -> L.Logger L.Hasura
  -> SourceConfig 'MSSQL
  -> ExceptT QErr IO EncJSON
  -> Maybe Text
  -> m (DiffTime, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutation reqId query fieldName _userInfo logger _sourceConfig tx _genSql =  do
  -- log the graphql query
  logQueryLog logger query Nothing reqId
  withElapsedTime
    $ trace ("MSSQL Mutation for root field " <>> fieldName)
    $ run tx

runSubscription
  :: ( MonadIO m
     )
  => SourceConfig 'MSSQL
  -> MultiplexedQuery 'MSSQL
  -> [(CohortId, CohortVariables)]
  -> m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runSubscription sourceConfig (NoMultiplex (name, query)) variables = do
  let pool = _mscConnectionPool sourceConfig
  withElapsedTime $ runExceptT $ for variables $ traverse $ const $
    fmap toResult $ run $ runJSONPathQuery pool query
  where
    toResult :: Text -> B.ByteString
    toResult = encodeUtf8 . addFieldName

    -- TODO: This should probably be generated from the database or should
    -- probably return encjson so that encJFromAssocList can be used
    addFieldName result =
      "{\"" <> G.unName name <> "\":" <> result <> "}"

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError
