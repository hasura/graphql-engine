{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Transport
--
-- Defines the MSSQL instance of 'BackendTransport' and how to
-- interact with the database for running queries, mutations, subscriptions,
-- and so on.
module Hasura.Backends.MSSQL.Instances.Transport () where

import Control.Exception.Safe (throwIO)
import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended
import Database.MSSQL.Transaction (forJsonQueryE)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Execute.QueryTags (withQueryTags)
import Hasura.Backends.MSSQL.Instances.Execute
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Backends.MSSQL.ToQuery
import Hasura.Base.Error
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
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance BackendTransport 'MSSQL where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = runSubscription
  runDBStreamingSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for MS-SQL sources."

newtype CohortResult = CohortResult (CohortId, Text)

instance J.FromJSON CohortResult where
  parseJSON = J.withObject "CohortResult" \o -> do
    cohortId <- o J..: "result_id"
    cohortData <- o J..: "result"
    pure $ CohortResult (cohortId, cohortData)

runQuery ::
  ( MonadIO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'MSSQL ->
  ExceptT QErr IO EncJSON ->
  Maybe (PreparedQuery 'MSSQL) ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql = do
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $
    trace ("MSSQL Query for root field " <>> fieldName) $
      run tx

runQueryExplain ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  DBStepInfo 'MSSQL ->
  m EncJSON
runQueryExplain (DBStepInfo _ _ _ action) = run action

runMutation ::
  ( MonadIO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'MSSQL ->
  ExceptT QErr IO EncJSON ->
  Maybe (PreparedQuery 'MSSQL) ->
  -- | Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runMutation reqId query fieldName _userInfo logger _sourceConfig tx _genSql = do
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId
  withElapsedTime $
    trace ("MSSQL Mutation for root field " <>> fieldName) $
      run tx

runSubscription ::
  MonadIO m =>
  SourceConfig 'MSSQL ->
  MultiplexedQuery 'MSSQL ->
  [(CohortId, CohortVariables)] ->
  m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runSubscription sourceConfig (MultiplexedQuery' reselect queryTags) variables = do
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
  MonadIO m =>
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
  textResult <- run $ mssqlRunReadOnly mssqlExecCtx $ forJsonQueryE defaultMSSQLTxErrorHandler query
  parsedResult <- parseResult textResult
  pure $ convertFromJSON parsedResult

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe (PreparedQuery 'MSSQL) ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId QueryLogKindDatabase
  where
    generatedQuery =
      preparedSql <&> \queryString ->
        GeneratedQuery queryString J.Null
