{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Transport () where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.ByteString                         as B
import qualified Database.ODBC.SQLServer                 as ODBC
import qualified Language.GraphQL.Draft.Syntax           as G

import           Data.String                             (fromString)
import           Data.Text.Encoding                      (encodeUtf8)
import           Data.Text.Extended

import qualified Hasura.Logging                          as L

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Instances.Execute
import           Hasura.Backends.MSSQL.ToQuery
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types
import           Hasura.Server.Types                     (RequestId)
import           Hasura.Session
import           Hasura.Tracing


instance BackendTransport 'MSSQL  where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = runSubscription

newtype CohortResult = CohortResult (CohortId, Text)

instance J.FromJSON CohortResult where
  parseJSON = J.withObject "CohortResult" \o -> do
    cohortId   <- o J..: "result_id"
    cohortData <- o J..: "result"
    pure $ CohortResult (cohortId, cohortData)

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
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql =  do
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime
    $ trace ("MSSQL Query for root field " <>> fieldName)
    $ run tx

runQueryExplain
  :: ( MonadIO m
     , MonadError QErr m
     )
  => DBStepInfo 'MSSQL
  -> m EncJSON
runQueryExplain (DBStepInfo _ _ _ action) = run action

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
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId
  withElapsedTime
    $ trace ("MSSQL Mutation for root field " <>> fieldName)
    $ run tx

runSubscription
  :: MonadIO m
  => SourceConfig 'MSSQL
  -> MultiplexedQuery 'MSSQL
  -> [(CohortId, CohortVariables)]
  -> m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runSubscription sourceConfig (MultiplexedQuery' reselect) variables = do
  let pool = _mscConnectionPool sourceConfig
      multiplexed = multiplexRootReselect variables reselect
      query = toQueryFlat $ fromSelect multiplexed
  withElapsedTime $ runExceptT $ executeMultiplexedQuery pool query

executeMultiplexedQuery
  :: MonadIO m
  => MSSQLPool
  -> ODBC.Query
  -> ExceptT QErr m [(CohortId, B.ByteString)]
executeMultiplexedQuery pool query = do
  let parseResult r = J.eitherDecodeStrict (encodeUtf8 r) `onLeft` \s -> throw400 ParseFailed (fromString s)
      convertFromJSON :: [CohortResult] -> [(CohortId, B.ByteString)]
      convertFromJSON = map \(CohortResult (cid, cresult)) -> (cid, encodeUtf8 cresult)
  textResult   <- run $ runJSONPathQuery pool query
  parsedResult <- parseResult textResult
  pure $ convertFromJSON parsedResult

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

mkQueryLog
  :: GQLReqUnparsed
  -> G.Name
  -> Maybe Text
  -> RequestId
  -> QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId Database
  where
    generatedQuery = preparedSql <&> \qs -> GeneratedQuery qs J.Null
