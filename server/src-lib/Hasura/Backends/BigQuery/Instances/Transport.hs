{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Transport () where

import qualified Data.Aeson                                 as J
import           Hasura.Backends.BigQuery.Instances.Execute ()
import           Hasura.Backends.MSSQL.Instances.Execute    ()
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                     (GeneratedQuery (..),
                                                             MonadQueryLog (..), QueryLog (..),
                                                             QueryLogKind (Database))
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import qualified Hasura.Logging                             as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Types                        (RequestId)
import           Hasura.Session
import           Hasura.Tracing
import qualified Hasura.Tracing                             as Tracing
import qualified Language.GraphQL.Draft.Syntax              as G

instance BackendTransport 'BigQuery  where
  runDBQuery = runQuery
  runDBQueryExplain = error "Not supported."
  runDBMutation = runMutation
  runDBSubscription = error "Not supported."

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
  -> SourceConfig 'BigQuery
  -> Tracing.TraceT (ExceptT QErr IO) EncJSON
  -> Maybe Text
  -> m (DiffTime, EncJSON)
  -- ^ Also return the time spent in the PG query; for telemetry.
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql = do
  -- log the generated SQL and the graphql query
  -- FIXME: fix logging by making logQueryLog expect something backend agnostic!
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $
    flip Tracing.interpTraceT tx $ \m -> run m

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

runMutation
  :: ( MonadError QErr m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name
  -> UserInfo
  -> L.Logger L.Hasura
  -> SourceConfig 'BigQuery
  -> Tracing.TraceT (ExceptT QErr IO) EncJSON
  -> Maybe Text
  -> m (DiffTime, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutation _reqId _query _fieldName _userInfo _logger _sourceConfig _tx _genSql =  -- do
  throw500 "BigQuery does not support mutations!"


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
