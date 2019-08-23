module Ops
  ( initCatalogSafe
  , cleanCatalog
  , execQuery
  ) where

import           Data.Time.Clock              (UTCTime)
import           Language.Haskell.TH.Syntax   (Q, TExp, unTypeQ)
import           Migrate                      (curCatalogVer)

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.Server.Query
import           Hasura.SQL.Types

import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text                    as T
import qualified Data.Yaml.TH                 as Y

import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q

initCatalogSafe
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => UTCTime -> m String
initCatalogSafe initTime =  do
  hdbCatalogExists <- liftTx $ Q.catchE defaultTxErrorHandler $
                      doesSchemaExist $ SchemaName "hdb_catalog"
  bool (initCatalogStrict True initTime) onCatalogExists hdbCatalogExists
  where
    onCatalogExists = do
      versionExists <- liftTx $ Q.catchE defaultTxErrorHandler $
                       doesVersionTblExist
                       (SchemaName "hdb_catalog") (TableName "hdb_version")
      bool (initCatalogStrict False initTime) (return initialisedMsg) versionExists

    initialisedMsg = "the state is already initialised"

    doesVersionTblExist sn tblN =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_tables
                WHERE schemaname = $1 AND tablename = $2)
               |] (sn, tblN) False

    doesSchemaExist sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM information_schema.schemata
                WHERE schema_name = $1
           )
                    |] (Identity sn) False

initCatalogStrict
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => Bool -> UTCTime -> m String
initCatalogStrict createSchema initTime =  do
  liftTx $ Q.catchE defaultTxErrorHandler $
    when createSchema $ do
      Q.unitQ "CREATE SCHEMA hdb_catalog" () False
      -- This is where the generated views and triggers are stored
      Q.unitQ "CREATE SCHEMA hdb_views" () False

  pgcryptoExtExists <- liftTx $
    Q.catchE defaultTxErrorHandler $ isExtAvailable "pgcrypto"

  if pgcryptoExtExists
    -- only if we created the schema, create the extension
    then when createSchema $ liftTx $ Q.unitQE needsPgCryptoExt
         "CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA public" () False
    else throw500 pgcryptoNotAvlMsg

  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.Discard () <- Q.multiQ $(Q.sqlFromFile "src-rsr/initialise.sql")
    return ()

  -- add default metadata
  void $ runQueryM metadataQuery

  setAllAsSystemDefined >> addVersion initTime
  return "successfully initialised"

  where
    metadataQuery =
      $(unTypeQ (Y.decodeFile "src-rsr/hdb_metadata.yaml" :: Q (TExp RQLQuery)))
    needsPgCryptoExt :: Q.PGTxErr -> QErr
    needsPgCryptoExt e@(Q.PGTxErr _ _ _ err) =
      case err of
        Q.PGIUnexpected _ -> (err500 PostgresError pgcryptoReqdMsg) { qeInternal = Just $ A.toJSON e }
        Q.PGIStatement pgErr ->
          case Q.edStatusCode pgErr of
            Just "42501" -> err500 PostgresError pgcryptoPermsMsg
            _ -> (err500 PostgresError pgcryptoReqdMsg) { qeInternal = Just $ A.toJSON e }

    addVersion modTime = liftTx $ Q.catchE defaultTxErrorHandler $
      Q.unitQ [Q.sql|
                INSERT INTO "hdb_catalog"."hdb_version"
                (version, upgraded_on) VALUES ($1, $2)
                |] (curCatalogVer, modTime) False

    isExtAvailable :: T.Text -> Q.Tx Bool
    isExtAvailable sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_catalog.pg_available_extensions
                WHERE name = $1
           )
                    |] (Identity sn) False


setAllAsSystemDefined :: (MonadTx m) => m ()
setAllAsSystemDefined = liftTx $ Q.catchE defaultTxErrorHandler $ do
  Q.unitQ "UPDATE hdb_catalog.hdb_table SET is_system_defined = 'true'" () False
  Q.unitQ "UPDATE hdb_catalog.hdb_relationship SET is_system_defined = 'true'" () False
  Q.unitQ "UPDATE hdb_catalog.hdb_permission SET is_system_defined = 'true'" () False

cleanCatalog :: (MonadTx m) => m ()
cleanCatalog = liftTx $ Q.catchE defaultTxErrorHandler $ do
  -- This is where the generated views and triggers are stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
  Q.unitQ "DROP SCHEMA hdb_catalog CASCADE" () False

execQuery
  :: ( MonadTx m, CacheRWM m, MonadIO m
     , UserInfoM m, HasHttpManager m, HasSQLGenCtx m
     )
  => BL.ByteString -> m BL.ByteString
execQuery queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  buildSchemaCacheStrict
  encJToLBS <$> runQueryM query

-- error messages
pgcryptoReqdMsg :: T.Text
pgcryptoReqdMsg =
  "pgcrypto extension is required, but could not install; encountered unknown postgres error"

pgcryptoPermsMsg :: T.Text
pgcryptoPermsMsg =
  "pgcrypto extension is required, but current user doesn't have permission to create it. "
  <> "Please grant superuser permission or setup initial schema via "
  <> "https://docs.hasura.io/1.0/graphql/manual/deployment/postgres-permissions.html"

pgcryptoNotAvlMsg :: T.Text
pgcryptoNotAvlMsg =
  "pgcrypto extension is required, but could not find the extension in the "
  <> "PostgreSQL server. Please make sure this extension is available."
