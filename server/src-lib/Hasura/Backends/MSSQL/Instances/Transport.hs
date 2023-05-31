{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Transport
--
-- Defines the MSSQL instance of 'BackendTransport' and how to
-- interact with the database for running queries, mutations, subscriptions,
-- and so on.
module Hasura.Backends.MSSQL.Instances.Transport () where

import Control.Exception.Safe (throwIO)
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended
import Database.MSSQL.Transaction (forJsonQueryE)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.QueryTags (withQueryTags)
import Hasura.Backends.MSSQL.Instances.Execute
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Backends.MSSQL.ToQuery
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance BackendTransport 'MSSQL where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = runSubscription
  runDBStreamingSubscription _ _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for MS-SQL sources."

newtype CohortResult = CohortResult (CohortId, Text)

instance J.FromJSON CohortResult where
  parseJSON = J.withObject "CohortResult" \o -> do
    cohortId <- o J..: "result_id"
    cohortData <- o J..: "result"
    pure $ CohortResult (cohortId, cohortData)

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig 'MSSQL ->
  OnBaseMonad (ExceptT QErr) (Maybe (AnyBackend ExecutionStats), EncJSON) ->
  Maybe (PreparedQuery 'MSSQL) ->
  ResolvedConnectionTemplate 'MSSQL ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _ sourceConfig tx genSql _ = do
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime
    $ newSpan ("MSSQL Query for root field " <>> fieldName)
    $ (<* attachSourceConfigAttributes @'MSSQL sourceConfig)
    $ fmap snd (run tx)

runQueryExplain ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  DBStepInfo 'MSSQL ->
  m EncJSON
runQueryExplain _ (DBStepInfo _ _ _ action _) = fmap arResult (run action)

runMutation ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  SourceConfig 'MSSQL ->
  OnBaseMonad (ExceptT QErr) EncJSON ->
  Maybe (PreparedQuery 'MSSQL) ->
  ResolvedConnectionTemplate 'MSSQL ->
  -- | Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runMutation reqId query fieldName _userInfo logger _ sourceConfig tx _genSql _ = do
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId
  withElapsedTime
    $ newSpan ("MSSQL Mutation for root field " <>> fieldName)
    $ (<* attachSourceConfigAttributes @'MSSQL sourceConfig)
    $ run tx

runSubscription ::
  (MonadIO m, MonadBaseControl IO m) =>
  SourceConfig 'MSSQL ->
  MultiplexedQuery 'MSSQL ->
  [(CohortId, CohortVariables)] ->
  ResolvedConnectionTemplate 'MSSQL ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runSubscription sourceConfig (MultiplexedQuery' reselect queryTags) variables _ = do
  let mssqlExecCtx = _mscExecCtx sourceConfig
      multiplexed = multiplexRootReselect variables reselect
      query = toQueryFlat (fromSelect multiplexed)
      -- Append query tags comment to the query. We cannot use 'toSQL' to convert
      -- QueryTagsComment to Query, because it escapes the query tags comment which
      -- will create a badly formatted query. Hence we use the 'rawUnescapedText' to
      -- append the comment without any escaping.
      queryWithQueryTags = query `withQueryTags` queryTags
  withElapsedTime $ runExceptT $ executeMultiplexedQuery mssqlExecCtx queryWithQueryTags

executeMultiplexedQuery ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLExecCtx ->
  ODBC.Query ->
  ExceptT QErr m [(CohortId, B.ByteString)]
executeMultiplexedQuery mssqlExecCtx query = do
  let parseResult r = J.eitherDecodeStrict (encodeUtf8 r) `onLeft` \s -> throw400 ParseFailed (fromString s)
      convertFromJSON :: [CohortResult] -> [(CohortId, B.ByteString)]
      convertFromJSON = map \(CohortResult (cid, cresult)) -> (cid, encodeUtf8 cresult)
  -- Because the 'query' will have a @FOR JSON@ clause at the toplevel it will
  -- be split across multiple rows, hence use of 'forJsonQueryE' which takes
  -- care of concatenating the results.
  textResult <- liftEitherM $ runExceptT $ mssqlRunReadOnly mssqlExecCtx $ forJsonQueryE defaultMSSQLTxErrorHandler query
  parsedResult <- parseResult textResult
  pure $ convertFromJSON parsedResult

run :: (MonadIO m, MonadBaseControl IO m, MonadError QErr m, MonadTrace m) => OnBaseMonad (ExceptT QErr) a -> m a
run = liftEitherM . runExceptT . runOnBaseMonad

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe (PreparedQuery 'MSSQL) ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  -- @QueryLogKindDatabase Nothing@ means that the backend doesn't support connection templates
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId (QueryLogKindDatabase Nothing)
  where
    generatedQuery =
      preparedSql <&> \queryString ->
        GeneratedQuery queryString J.Null
