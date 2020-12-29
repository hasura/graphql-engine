module Hasura.RQL.DDL.Schema.Source where

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Hasura.Backends.Postgres.Connection
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.Types

import qualified Data.Environment                    as Env
import qualified Database.PG.Query                   as Q

mkPgSourceResolver :: Q.PGLogger -> SourceResolver
mkPgSourceResolver pgLogger config = runExceptT do
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
  => SourceConfiguration -> m (Either QErr ResolvedPGSource)
resolveSource config = runExceptT do
  sourceResolver <- getSourceResolver
  sourceConfig <- liftEitherM $ liftIO $ sourceResolver config

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
