module Hasura.RQL.DDL.Schema.Source where

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Text.Extended

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
  let PostgresSourceConnInfo urlConf connSettings = _scConnectionInfo config
      PostgresPoolSettings maxConns idleTimeout retries = connSettings
  urlText <- resolveUrlConf env urlConf
  let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
      connParams = Q.defaultConnParams{ Q.cpIdleTime = idleTimeout
                                      , Q.cpConns = maxConns
                                      }
  pgPool <- liftIO $ Q.initPGPool connInfo connParams pgLogger
  let pgExecCtx = mkPGExecCtx Q.ReadCommitted pgPool
  pure $ PGSourceConfig pgExecCtx connInfo Nothing

resolveSource
  :: (MonadIO m, MonadBaseControl IO m, MonadResolveSource m)
  => SourceName -> SourceConfiguration -> m (Either QErr ResolvedPGSource)
resolveSource name config = runExceptT do
  sourceResolver <- getSourceResolver
  sourceConfig <- liftEitherM $ liftIO $ sourceResolver name config

  (tablesMeta, functionsMeta, pgScalars) <- runLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite $ do
    initSource
    tablesMeta    <- fetchTableMetadata
    functionsMeta <- fetchFunctionMetadata
    pgScalars     <- fetchPgScalars
    pure (tablesMeta, functionsMeta, pgScalars)
  pure $ ResolvedPGSource sourceConfig tablesMeta functionsMeta pgScalars

initSource :: MonadTx m => m ()
initSource = do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
  sourceVersionTableExist <- doesTableExist "hdb_catalog" "hdb_source_catalog_version"
     -- Fresh database
  if | not hdbCatalogExist -> liftTx do
         Q.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
         enablePgcryptoExtension
         initPgSourceCatalog
     -- Only 'hdb_catalog' schema defined
     | not sourceVersionTableExist && not eventLogTableExist ->
         liftTx initPgSourceCatalog
     -- Source is initialised by pre multisource support servers
     | not sourceVersionTableExist && eventLogTableExist ->
         liftTx createVersionTable
     | otherwise -> migrateSourceCatalog
  where
    initPgSourceCatalog = do
      () <- Q.multiQE defaultTxErrorHandler $(Q.sqlFromFile "src-rsr/init_pg_source.sql")
      setSourceCatalogVersion

    createVersionTable = do
      () <- Q.multiQE defaultTxErrorHandler
        [Q.sql|
           CREATE TABLE hdb_catalog.hdb_source_catalog_version(
             version TEXT NOT NULL,
             upgraded_on TIMESTAMPTZ NOT NULL
           );

           CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
           ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));
        |]
      setSourceCatalogVersion

    migrateSourceCatalog = do
      version <- getSourceCatalogVersion
      case version of
        "1" -> pure ()
        _   -> throw500 $ "unexpected source catalog version: " <> version

currentSourceCatalogVersion :: Text
currentSourceCatalogVersion = "1"

setSourceCatalogVersion :: MonadTx m => m ()
setSourceCatalogVersion = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
  INSERT INTO hdb_catalog.hdb_source_catalog_version(version, upgraded_on)
    VALUES ($1, NOW())
   ON CONFLICT ((version IS NOT NULL))
   DO UPDATE SET version = $1, upgraded_on = NOW()
  |] (Identity currentSourceCatalogVersion) False

getSourceCatalogVersion :: MonadTx m => m Text
getSourceCatalogVersion = liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
  [Q.sql| SELECT version FROM hdb_catalog.hdb_source_catalog_version |] () False

--- Metadata APIs related
runAddPgSource
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => AddPgSource -> m EncJSON
runAddPgSource (AddPgSource name sourceConfig) = do
  let PostgresSourceConnInfo url connSettings = _scConnectionInfo sourceConfig
  sources <- scPostgres <$> askSchemaCache
  onJust (HM.lookup name sources) $ const $
    throw400 AlreadyExists $ "postgres source with name " <> name <<> " already exists"
  buildSchemaCacheFor (MOSource name)
    $ MetadataModifier
    $ metaSources %~ OMap.insert name (mkSourceMetadata name url connSettings)
  pure successMsg

runDropPgSource
  :: (MonadError QErr m, CacheRWM m, MonadIO m, MonadBaseControl IO m, MetadataM m)
  => DropPgSource -> m EncJSON
runDropPgSource (DropPgSource name cascade) = do
  sourceConfig <- _pcConfiguration <$> askPGSourceCache name
  sc <- askSchemaCache
  let indirectDeps = filter (not . isDirectDep) $
                     getDependentObjs sc (SOSource name)

  when (not cascade && indirectDeps /= []) $ reportDepsExt indirectDeps []

  metadataModifier <- execWriterT $ do
    mapM_ (purgeDependentObject >=> tell) indirectDeps
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
    isDirectDep = \case
      SOSource s      -> s == name
      SOSourceObj s _ -> s == name
      _               -> False
