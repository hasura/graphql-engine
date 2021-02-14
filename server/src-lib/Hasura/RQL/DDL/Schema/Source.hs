module Hasura.RQL.DDL.Schema.Source where

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Text.Extended
import           Data.Typeable                       (cast)

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.Types

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as HM
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Database.PG.Query                   as Q

mkPgSourceResolver :: Q.PGLogger -> SourceResolver
mkPgSourceResolver pgLogger _ config = runExceptT do
  env <- lift Env.getEnvironment
  let PostgresSourceConnInfo urlConf connSettings = _pccConnectionInfo config
      PostgresPoolSettings maxConns idleTimeout retries = connSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
      connParams = Q.defaultConnParams{ Q.cpIdleTime = idleTimeout
                                      , Q.cpConns = maxConns
                                      }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx Q.ReadCommitted pgPool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing

--- Metadata APIs related
runAddPgSource
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => AddPgSource -> m EncJSON
runAddPgSource (AddPgSource name sourceConfig) = do
  let sourceConnConfig = PostgresConnConfiguration (_pccConnectionInfo sourceConfig) mempty
  sources <- scPostgres <$> askSchemaCache
  onJust (HM.lookup name sources) $ const $
    throw400 AlreadyExists $ "postgres source with name " <> name <<> " already exists"
  buildSchemaCacheFor (MOSource name)
    $ MetadataModifier
    $ metaSources %~ OMap.insert name (mkSourceMetadata @'Postgres name sourceConnConfig)
  pure successMsg

runDropPgSource
  :: (MonadError QErr m, CacheRWM m, MonadIO m, MonadBaseControl IO m, MetadataM m)
  => DropPgSource -> m EncJSON
runDropPgSource (DropPgSource name cascade) = do
  sourceConfig <- askSourceConfig name
  sc <- askSchemaCache
  let indirectDeps = mapMaybe getIndirectDep $
                     getDependentObjs sc (SOSource name)

  when (not cascade && indirectDeps /= []) $ reportDepsExt (map (SOSourceObj name) indirectDeps) []

  metadataModifier <- execWriterT $ do
    mapM_ (purgeDependentObject name >=> tell) indirectDeps
    tell $ MetadataModifier $ metaSources %~ OMap.delete name

  buildSchemaCacheFor (MOSource name) metadataModifier
  -- Clean traces of Hasura in source database
  liftEitherM $ runPgSourceWriteTx sourceConfig $ do
    hdbMetadataTableExist <- doesTableExist "hdb_catalog" "hdb_metadata"
    eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
        -- If "hdb_metadata" and "event_log" tables found in the "hdb_catalog" schema
        -- then this infers the source is being used as default potgres source (--database-url option).
        -- In this case don't drop any thing in the catalog schema.
    if | hdbMetadataTableExist && eventLogTableExist -> pure ()
       -- Otherwise, if only "hdb_metadata" table exist, then this infers the source is
       -- being used as metadata storage (--metadata-database-url option). In this case
       -- drop only source related tables and not "hdb_catalog" schema
       | hdbMetadataTableExist ->
         Q.multiQE defaultTxErrorHandler $(Q.sqlFromFile "src-rsr/drop_pg_source.sql")
       -- Otherwise, drop "hdb_catalog" schema.
       | otherwise -> dropHdbCatalogSchema

  -- Destory postgres source connection
  liftIO $ _pecDestroyConn $ _pscExecCtx sourceConfig
  pure successMsg
  where
    getIndirectDep :: SchemaObjId -> Maybe (SourceObjId 'Postgres)
    getIndirectDep = \case
      SOSourceObj s o -> if s == name then Nothing else cast o -- consider only postgres backend dependencies
      _               -> Nothing
