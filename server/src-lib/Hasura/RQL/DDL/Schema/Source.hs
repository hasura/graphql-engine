module Hasura.RQL.DDL.Schema.Source where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson

import qualified Data.Environment    as Env
import qualified Data.HashMap.Strict as HM
import qualified Database.PG.Query   as Q

resolveSource
  :: (MonadIO m, HasDefaultSource m)
  => Env.Environment -> SourceMetadata -> m (Either QErr ResolvedSource)
resolveSource env (SourceMetadata _ tables functions config) = runExceptT do
  sourceConfig <- case config of
    SCDefault -> askDefaultSource
    SCCustom (SourceCustomConfiguration urlConf connSettings) -> do
      let SourceConnSettings maxConns idleTimeout retries = connSettings
      urlText <- resolveUrlConf env urlConf
      let connInfo = Q.ConnInfo retries $ Q.CDDatabaseURI $ txtToBs urlText
          connParams = Q.defaultConnParams{ Q.cpIdleTime = idleTimeout
                                          , Q.cpConns = maxConns
                                          }
      pgPool <- liftIO $ Q.initPGPool connInfo connParams (\_ -> pure ()) -- FIXME? Pg logger
      let pgExecCtx = mkPGExecCtx Q.Serializable pgPool
      pure $ PGSourceConfig pgExecCtx connInfo

  (tablesMeta, functionsMeta, pgScalars) <- runLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite $ do
    initSource
    tablesMeta <- fetchTableMetadataFromPgSource
    functionsMeta <- fetchFunctionMetadataFromPgSource allFunctions
    pgScalars <- fetchPgScalars
    pure (tablesMeta, functionsMeta, pgScalars)
  pure $ ResolvedSource sourceConfig tablesMeta functionsMeta pgScalars
  where
    allFunctions =
      let computedFieldFunctions = concatMap
            (map (_cfdFunction . _cfmDefinition) . HM.elems . _tmComputedFields)
            (HM.elems tables)
      in computedFieldFunctions <> map _fmFunction (HM.elems functions)

initSource :: MonadTx m => m ()
initSource = do
  hdbCatalogExist <- doesSchemaExist "hdb_catalog"
  eventLogTableExist <- doesTableExist "hdb_catalog" "event_log"
  sourceVersionTableExist <- doesTableExist "hdb_catalog" "hdb_source_catalog_version"
  if | not hdbCatalogExist -> liftTx do
         Q.unitQE defaultTxErrorHandler "CREATE SCHEMA hdb_catalog" () False
         pgcryptoAvailable <- isExtensionAvailable "pgcrypto"
         if pgcryptoAvailable then createPgcryptoExtension
           else throw400 Unexpected $
             "pgcrypto extension is required, but could not find the extension in the "
             <> "PostgreSQL server. Please make sure this extension is available."
         initPgSourceCatalog
     | not sourceVersionTableExist && eventLogTableExist -> do
         liftTx createVersionTable
     | otherwise -> migrateSourceCatalog

  setSourceCatalogVersion
  where
    initPgSourceCatalog =
      Q.multiQE defaultTxErrorHandler $(Q.sqlFromFile "src-rsr/init_pg_source.sql")
    createVersionTable =
      Q.multiQE defaultTxErrorHandler
      [Q.sql|
         CREATE TABLE hdb_catalog.hdb_source_catalog_version(
           version TEXT NOT NULL,
           upgraded_on TIMESTAMPTZ NOT NULL
         );

         CREATE UNIQUE INDEX hdb_source_catalog_version_one_row
         ON hdb_catalog.hdb_source_catalog_version((version IS NOT NULL));
      |]

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


-- | Fetch all user tables with metadata
fetchTableMetadataFromPgSource :: (MonadTx m) => m PostgresTablesMetadata
fetchTableMetadataFromPgSource = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_table_metadata.sql")
             () True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ info) -> (QualifiedObject schema table, info)

-- | Fetch Postgres metadata for the given functions
fetchFunctionMetadataFromPgSource
  :: (MonadTx m) => [QualifiedFunction] -> m PostgresFunctionsMetadata
fetchFunctionMetadataFromPgSource functionList = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_function_metadata.sql")
             (Identity $ Q.AltJ $ toJSON functionList) True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ infos) -> (QualifiedObject schema table, infos)

fetchPgScalars :: MonadTx m => m (HashSet PGScalarType)
fetchPgScalars =
  liftTx $ Q.getAltJ . runIdentity . Q.getRow
  <$> Q.withQE defaultTxErrorHandler
  [Q.sql|
    SELECT coalesce(json_agg(typname), '[]')
    FROM pg_catalog.pg_type where typtype = 'b'
   |] () True

--- Metadata APIs related
runAddPgSource
  :: (MonadError QErr m, CacheRWM m)
  => AddPgSource -> m EncJSON
runAddPgSource (AddPgSource name url connSettings) = do
  sources <- scPostgres <$> askSchemaCache
  onJust (HM.lookup name sources) $ const $
    throw400 AlreadyExists $ "postgres source with name " <> name <<> " already exists"
  buildSchemaCacheFor (MOSource name)
    $ MetadataModifier
    $ metaSources %~ HM.insert name (mkSourceMetadata name url connSettings)
  pure successMsg
