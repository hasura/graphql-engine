{-# LANGUAGE TemplateHaskell #-}

-- | The RQL query ('/v2/query')
module Hasura.Server.API.V2Query
  ( RQLQuery,
    queryModifiesSchema,
    runQuery,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Environment qualified as Env
import Data.Text qualified as T
import GHC.Generics.Extended (constrName)
import Hasura.Backends.BigQuery.DDL.RunSQL qualified as BigQuery
import Hasura.Backends.MSSQL.DDL.RunSQL qualified as MSSQL
import Hasura.Backends.MySQL.SQL qualified as MySQL
import Hasura.Backends.Postgres.DDL.RunSQL qualified as Postgres
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DML.Count
import Hasura.RQL.DML.Delete
import Hasura.RQL.DML.Insert
import Hasura.RQL.DML.Select
import Hasura.RQL.DML.Types
import Hasura.RQL.DML.Update
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Run
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.SQL.Backend
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP

data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount !CountQuery
  | RQRunSql !Postgres.RunSQL
  | RQMssqlRunSql !MSSQL.MSSQLRunSQL
  | RQCitusRunSql !Postgres.RunSQL
  | RQMysqlRunSql !MySQL.RunSQL
  | RQBigqueryRunSql !BigQuery.BigQueryRunSQL
  | RQBigqueryDatabaseInspection !BigQuery.BigQueryRunSQL
  | RQBulk ![RQLQuery]
  deriving (Generic)

$( deriveFromJSON
     defaultOptions
       { constructorTagModifier = snakeCase . drop 2,
         sumEncoding = TaggedObject "type" "args"
       }
     ''RQLQuery
 )

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m
  ) =>
  Env.Environment ->
  InstanceId ->
  UserInfo ->
  RebuildableSchemaCache ->
  HTTP.Manager ->
  ServerConfigCtx ->
  RQLQuery ->
  m (EncJSON, RebuildableSchemaCache)
runQuery env instanceId userInfo schemaCache httpManager serverConfigCtx rqlQuery = do
  when ((_sccReadOnlyMode serverConfigCtx == ReadOnlyModeEnabled) && queryModifiesUserDB rqlQuery) $
    throw400 NotSupported "Cannot run write queries when read-only mode is enabled"

  (metadata, currentResourceVersion) <- Tracing.trace "fetchMetadata" fetchMetadata
  result <-
    runQueryM env rqlQuery & \x -> do
      ((js, meta), rsc, ci) <-
        x & runMetadataT metadata
          & runCacheRWT schemaCache
          & peelRun runCtx
          & runExceptT
          & liftEitherM
      pure (js, rsc, ci, meta)
  withReload currentResourceVersion result
  where
    runCtx = RunCtx userInfo httpManager serverConfigCtx

    withReload currentResourceVersion (result, updatedCache, invalidations, updatedMetadata) = do
      when (queryModifiesSchema rqlQuery) $ do
        case _sccMaintenanceMode serverConfigCtx of
          MaintenanceModeDisabled -> do
            -- set modified metadata in storage
            newResourceVersion <-
              Tracing.trace "setMetadata" $
                setMetadata currentResourceVersion updatedMetadata
            -- notify schema cache sync
            Tracing.trace "notifySchemaCacheSync" $
              notifySchemaCacheSync newResourceVersion instanceId invalidations
          MaintenanceModeEnabled () ->
            throw500 "metadata cannot be modified in maintenance mode"
      pure (result, updatedCache)

queryModifiesSchema :: RQLQuery -> Bool
queryModifiesSchema = \case
  RQInsert _ -> False
  RQSelect _ -> False
  RQUpdate _ -> False
  RQDelete _ -> False
  RQCount _ -> False
  RQRunSql q -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQCitusRunSql q -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQMssqlRunSql q -> MSSQL.isSchemaCacheBuildRequiredRunSQL q
  RQMysqlRunSql _ -> False
  RQBigqueryRunSql _ -> False
  RQBigqueryDatabaseInspection _ -> False
  RQBulk l -> any queryModifiesSchema l

runQueryM ::
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    UserInfoM m,
    CacheRWM m,
    HasServerConfigCtx m,
    Tracing.MonadTrace m,
    MetadataM m,
    MonadQueryTags m
  ) =>
  Env.Environment ->
  RQLQuery ->
  m EncJSON
runQueryM env rq = Tracing.trace (T.pack $ constrName rq) $ case rq of
  RQInsert q -> runInsert q
  RQSelect q -> runSelect q
  RQUpdate q -> runUpdate q
  RQDelete q -> runDelete q
  RQCount q -> runCount q
  RQRunSql q -> Postgres.runRunSQL @'Vanilla q
  RQMssqlRunSql q -> MSSQL.runSQL q
  RQMysqlRunSql q -> MySQL.runSQL q
  RQCitusRunSql q -> Postgres.runRunSQL @'Citus q
  RQBigqueryRunSql q -> BigQuery.runSQL q
  RQBigqueryDatabaseInspection q -> BigQuery.runDatabaseInspection q
  RQBulk l -> encJFromList <$> indexedMapM (runQueryM env) l

queryModifiesUserDB :: RQLQuery -> Bool
queryModifiesUserDB = \case
  RQInsert _ -> True
  RQSelect _ -> False
  RQUpdate _ -> True
  RQDelete _ -> True
  RQCount _ -> False
  RQRunSql _ -> True
  RQCitusRunSql _ -> True
  RQMssqlRunSql _ -> True
  RQMysqlRunSql _ -> True
  RQBigqueryRunSql _ -> True
  RQBigqueryDatabaseInspection _ -> False
  RQBulk q -> any queryModifiesUserDB q
