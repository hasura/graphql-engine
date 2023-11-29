{-# LANGUAGE ViewPatterns #-}

-- | The RQL query ('/v2/query')
module Hasura.Server.API.V2Query
  ( RQLQuery,
    queryModifiesSchema,
    runQuery,
  )
where

import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Lens (preview, _Right)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text qualified as T
import GHC.Generics.Extended (constrName)
import Hasura.App.State
import Hasura.Backends.BigQuery.DDL.RunSQL qualified as BigQuery
import Hasura.Backends.DataConnector.Adapter.RunSQL qualified as DataConnector
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorName, mkDataConnectorName)
import Hasura.Backends.MSSQL.DDL.RunSQL qualified as MSSQL
import Hasura.Backends.Postgres.DDL.RunSQL qualified as Postgres
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.DML.Count
import Hasura.RQL.DML.Delete
import Hasura.RQL.DML.Insert
import Hasura.RQL.DML.Select
import Hasura.RQL.DML.Types
  ( CountQuery,
    DeleteQuery,
    InsertQuery,
    SelectQuery,
    UpdateQuery,
  )
import Hasura.RQL.DML.Update
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache (MetadataWithResourceVersion (MetadataWithResourceVersion), SchemaCache (scInconsistentObjs))
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as GQL

data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount !CountQuery
  | RQRunSql !Postgres.RunSQL
  | RQMssqlRunSql !MSSQL.MSSQLRunSQL
  | RQCitusRunSql !Postgres.RunSQL
  | RQCockroachRunSql !Postgres.RunSQL
  | RQBigqueryRunSql !BigQuery.BigQueryRunSQL
  | RQDataConnectorRunSql !DataConnectorName !DataConnector.DataConnectorRunSQL
  | RQBigqueryDatabaseInspection !BigQuery.BigQueryRunSQL
  | RQBulk ![RQLQuery]
  | -- | A variant of 'RQBulk' that runs a bulk of read-only queries concurrently.
    --   Asserts that queries on this lists are not modifying the schema.
    --
    --   This is mainly used by the graphql-engine console.
    RQConcurrentBulk [RQLQuery]
  deriving (Generic)

-- | This instance has been written by hand so that "wildcard" prefixes of _run_sql can be delegated to data connectors.
instance FromJSON RQLQuery where
  parseJSON = withObject "RQLQuery" \o -> do
    t <- o .: "type"
    let args :: forall a. (FromJSON a) => Parser a
        args = o .: "args"
        dcNameFromRunSql = T.stripSuffix "_run_sql" >=> GQL.mkName >=> preview _Right . mkDataConnectorName
    case t of
      "insert" -> RQInsert <$> args
      "select" -> RQSelect <$> args
      "update" -> RQUpdate <$> args
      "delete" -> RQDelete <$> args
      "count" -> RQCount <$> args
      -- Optionally, we can specify a `pg_` prefix. This primarily makes some
      -- string interpolation easier in the cross-backend tests.
      "run_sql" -> RQRunSql <$> args
      "pg_run_sql" -> RQRunSql <$> args
      "mssql_run_sql" -> RQMssqlRunSql <$> args
      "citus_run_sql" -> RQCitusRunSql <$> args
      "cockroach_run_sql" -> RQCockroachRunSql <$> args
      "bigquery_run_sql" -> RQBigqueryRunSql <$> args
      (dcNameFromRunSql -> Just t') -> RQDataConnectorRunSql t' <$> args
      "bigquery_database_inspection" -> RQBigqueryDatabaseInspection <$> args
      "bulk" -> RQBulk <$> args
      "concurrent_bulk" -> RQConcurrentBulk <$> args
      _ -> fail $ "Unrecognised RQLQuery type: " <> T.unpack t

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m,
    ProvidesHasuraServices m,
    UserInfoM m
  ) =>
  AppContext ->
  RebuildableSchemaCache ->
  RQLQuery ->
  m (EncJSON, RebuildableSchemaCache)
runQuery appContext schemaCache rqlQuery = do
  AppEnv {..} <- askAppEnv
  when ((appEnvEnableReadOnlyMode == ReadOnlyModeEnabled) && queryModifiesUserDB rqlQuery)
    $ throw400 NotSupported "Cannot run write queries when read-only mode is enabled"

  let dynamicConfig = buildCacheDynamicConfig appContext
  MetadataWithResourceVersion metadata currentResourceVersion <- Tracing.newSpan "fetchMetadata" $ liftEitherM fetchMetadata
  ((result, updatedMetadata), modSchemaCache, invalidations, sourcesIntrospection, schemaRegistryAction) <-
    runQueryM (acSQLGenCtx appContext) rqlQuery
      -- We can use defaults here unconditionally, since there is no MD export function in V2Query
      & runMetadataT metadata (acMetadataDefaults appContext)
      & runCacheRWT dynamicConfig schemaCache
  if queryModifiesSchema rqlQuery
    then case appEnvEnableMaintenanceMode of
      MaintenanceModeDisabled -> do
        -- set modified metadata in storage
        newResourceVersion <-
          Tracing.newSpan "setMetadata"
            $ liftEitherM
            $ setMetadata currentResourceVersion updatedMetadata

        (_, modSchemaCache', _, _, _) <-
          Tracing.newSpan "setMetadataResourceVersionInSchemaCache"
            $ setMetadataResourceVersionInSchemaCache newResourceVersion
            & runCacheRWT dynamicConfig modSchemaCache

        -- save sources introspection to stored-introspection DB
        Tracing.newSpan "storeSourcesIntrospection"
          $ saveSourcesIntrospection (_lsLogger appEnvLoggers) sourcesIntrospection newResourceVersion

        -- run schema registry action
        Tracing.newSpan "runSchemaRegistryAction"
          $ for_ schemaRegistryAction
          $ \action -> do
            liftIO $ action newResourceVersion (scInconsistentObjs (lastBuiltSchemaCache modSchemaCache')) updatedMetadata

        -- notify schema cache sync
        Tracing.newSpan "notifySchemaCacheSync"
          $ liftEitherM
          $ notifySchemaCacheSync newResourceVersion appEnvInstanceId invalidations

        pure (result, modSchemaCache')
      MaintenanceModeEnabled () ->
        throw500 "metadata cannot be modified in maintenance mode"
    else pure (result, modSchemaCache)

queryModifiesSchema :: RQLQuery -> Bool
queryModifiesSchema = \case
  RQInsert _ -> False
  RQSelect _ -> False
  RQUpdate _ -> False
  RQDelete _ -> False
  RQCount _ -> False
  RQRunSql q -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQCitusRunSql q -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQCockroachRunSql q -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQMssqlRunSql q -> MSSQL.isSchemaCacheBuildRequiredRunSQL q
  RQBigqueryRunSql _ -> False
  RQDataConnectorRunSql _ _ -> False
  RQBigqueryDatabaseInspection _ -> False
  RQBulk l -> any queryModifiesSchema l
  RQConcurrentBulk l -> any queryModifiesSchema l

runQueryM ::
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    UserInfoM m,
    CacheRWM m,
    Tracing.MonadTrace m,
    MetadataM m,
    MonadQueryTags m
  ) =>
  SQLGenCtx ->
  RQLQuery ->
  m EncJSON
runQueryM sqlGen rq = Tracing.newSpan (T.pack $ constrName rq) $ case rq of
  RQInsert q -> runInsert sqlGen q
  RQSelect q -> runSelect sqlGen q
  RQUpdate q -> runUpdate sqlGen q
  RQDelete q -> runDelete sqlGen q
  RQCount q -> runCount q
  RQRunSql q -> Postgres.runRunSQL @'Vanilla sqlGen q
  RQMssqlRunSql q -> MSSQL.runSQL q
  RQCitusRunSql q -> Postgres.runRunSQL @'Citus sqlGen q
  RQCockroachRunSql q -> Postgres.runRunSQL @'Cockroach sqlGen q
  RQBigqueryRunSql q -> BigQuery.runSQL q
  RQDataConnectorRunSql t q -> DataConnector.runSQL t q
  RQBigqueryDatabaseInspection q -> BigQuery.runDatabaseInspection q
  RQBulk l -> encJFromList <$> indexedMapM (runQueryM sqlGen) l
  RQConcurrentBulk l -> do
    when (queryModifiesSchema rq)
      $ throw500 "Only read-only queries are allowed in a concurrent_bulk"
    encJFromList <$> mapConcurrently (runQueryM sqlGen) l

queryModifiesUserDB :: RQLQuery -> Bool
queryModifiesUserDB = \case
  RQInsert _ -> True
  RQSelect _ -> False
  RQUpdate _ -> True
  RQDelete _ -> True
  RQCount _ -> False
  RQRunSql runsql -> not (Postgres.isReadOnly runsql)
  RQCitusRunSql runsql -> not (Postgres.isReadOnly runsql)
  RQCockroachRunSql runsql -> not (Postgres.isReadOnly runsql)
  RQMssqlRunSql _ -> True
  RQBigqueryRunSql _ -> True
  RQDataConnectorRunSql _ _ -> True
  RQBigqueryDatabaseInspection _ -> False
  RQBulk q -> any queryModifiesUserDB q
  RQConcurrentBulk _ -> False
