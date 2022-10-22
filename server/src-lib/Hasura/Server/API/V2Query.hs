-- | The RQL query ('/v2/query')

module Hasura.Server.API.V2Query where

import           Hasura.Prelude

import qualified Data.Environment                    as Env
import qualified Network.HTTP.Client                 as HTTP

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Hasura.Backends.BigQuery.DDL.RunSQL as BigQuery
import qualified Hasura.Backends.MSSQL.DDL.RunSQL    as MSSQL
import qualified Hasura.Backends.Postgres.DDL.RunSQL as Postgres
import qualified Hasura.Tracing                      as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Types
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Types
import           Hasura.Server.Version               (HasVersion)
import           Hasura.Session


data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount  !CountQuery
  | RQRunSql !Postgres.RunSQL
  | RQMssqlRunSql !MSSQL.MSSQLRunSQL
  | RQCitusRunSql !Postgres.RunSQL
  | RQBigqueryRunSql !BigQuery.BigQueryRunSQL
  | RQBigqueryDatabaseInspection !BigQuery.BigQueryRunSQL
  | RQBulk ![RQLQuery]
  deriving (Show)

$(deriveFromJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)


runQuery
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , Tracing.MonadTrace m
     , MonadMetadataStorage m
     , MonadResolveSource m
     )
  => Env.Environment
  -> InstanceId
  -> UserInfo
  -> RebuildableSchemaCache
  -> HTTP.Manager
  -> ServerConfigCtx
  -> RQLQuery
  -> m (EncJSON, RebuildableSchemaCache)
runQuery env instanceId userInfo schemaCache httpManager serverConfigCtx rqlQuery = do
  (metadata, currentResourceVersion) <- fetchMetadata
  result <- runQueryM env rqlQuery & Tracing.interpTraceT \x -> do
    (((js, tracemeta), meta), rsc, ci) <-
         x & runMetadataT metadata
           & runCacheRWT schemaCache
           & peelRun runCtx
           & runExceptT
           & liftEitherM
    pure ((js, rsc, ci, meta), tracemeta)
  withReload currentResourceVersion result
  where
    runCtx = RunCtx userInfo httpManager serverConfigCtx

    withReload currentResourceVersion (result, updatedCache, invalidations, updatedMetadata) = do
      when (queryModifiesSchema rqlQuery) $ do
        case _sccMaintenanceMode serverConfigCtx of
          MaintenanceModeDisabled -> do
            -- set modified metadata in storage
            newResourceVersion <- setMetadata currentResourceVersion updatedMetadata
            -- notify schema cache sync
            notifySchemaCacheSync newResourceVersion instanceId invalidations
          MaintenanceModeEnabled ->
            throw500 "metadata cannot be modified in maintenance mode"
      pure (result, updatedCache)

queryModifiesSchema :: RQLQuery -> Bool
queryModifiesSchema = \case
  RQInsert _                     -> False
  RQSelect _                     -> False
  RQUpdate _                     -> False
  RQDelete _                     -> False
  RQCount  _                     -> False
  RQRunSql q                     -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQCitusRunSql q                -> Postgres.isSchemaCacheBuildRequiredRunSQL q
  RQMssqlRunSql q                -> MSSQL.sqlContainsDDLKeyword $ MSSQL._mrsSql q
  RQBigqueryRunSql _             -> False
  RQBigqueryDatabaseInspection _ -> False
  RQBulk l                       -> any queryModifiesSchema l

runQueryM
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , MonadBaseControl IO m
     , UserInfoM m
     , CacheRWM m
     , HasServerConfigCtx m
     , Tracing.MonadTrace m
     , MetadataM m
     )
  => Env.Environment -> RQLQuery -> m EncJSON
runQueryM env = \case
  RQInsert q                     -> runInsert q
  RQSelect q                     -> runSelect q
  RQUpdate q                     -> runUpdate q
  RQDelete q                     -> runDelete q
  RQCount  q                     -> runCount q
  RQRunSql q                     -> Postgres.runRunSQL @'Vanilla q
  RQMssqlRunSql q                -> MSSQL.runSQL q
  RQCitusRunSql q                -> Postgres.runRunSQL @'Citus q
  RQBigqueryRunSql q             -> BigQuery.runSQL q
  RQBigqueryDatabaseInspection q -> BigQuery.runDatabaseInspection q
  RQBulk   l                     -> encJFromList <$> indexedMapM (runQueryM env) l
