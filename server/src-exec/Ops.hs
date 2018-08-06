{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ops
  ( initCatalogSafe
  , cleanCatalog
  , execQuery
  ) where

import           Migrate                     (curCatalogVer)
import           TH

import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.Server.Query
import           Hasura.SQL.Types

import qualified Database.PG.Query           as Q

import           Data.Time.Clock             (UTCTime)

import qualified Data.Aeson                  as A
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T

initCatalogSafe :: UTCTime -> Q.TxE QErr String
initCatalogSafe initTime =  do
  hdbCatalogExists <- Q.catchE defaultTxErrorHandler $
                      doesSchemaExist $ SchemaName "hdb_catalog"
  bool (initCatalogStrict True initTime) onCatalogExists hdbCatalogExists
  where
    onCatalogExists = do
      versionExists <- Q.catchE defaultTxErrorHandler $
                       doesVersionTblExist
                       (SchemaName "hdb_catalog") (TableName "hdb_version")
      bool (initCatalogStrict False initTime) (return initialisedMsg) versionExists

    initialisedMsg = "initialise: the state is already initialised"

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

initCatalogStrict :: Bool -> UTCTime -> Q.TxE QErr String
initCatalogStrict createSchema initTime =  do
  Q.catchE defaultTxErrorHandler $ do

    when createSchema $ do
      Q.unitQ "CREATE SCHEMA hdb_catalog" () False
      -- This is where the generated views and triggers are stored
      Q.unitQ "CREATE SCHEMA hdb_views" () False

    flExtExists <- isExtInstalled "first_last_agg"

    if flExtExists then Q.unitQ "CREATE EXTENSION first_last_agg SCHEMA hdb_catalog" () False
    else Q.multiQ $(Q.sqlFromFile "src-rsr/first_last.sql") >>= \(Q.Discard _) -> return ()

    Q.Discard () <- Q.multiQ $(Q.sqlFromFile "src-rsr/init_catalog.sql")
    initDefaultViews

  -- Build the metadata query
  tx <- liftEither $ buildTxAny adminUserInfo emptySchemaCache metadataQuery

  -- Execute the query
  void $ snd <$> tx
  setAsSystemDefined >> addVersion initTime
  return "initialise: successfully initialised"
  where
    addVersion modTime = Q.catchE defaultTxErrorHandler $
      Q.unitQ [Q.sql|
                INSERT INTO "hdb_catalog"."hdb_version" VALUES ($1, $2)
                |] (curCatalogVer, modTime) False

    setAsSystemDefined = Q.catchE defaultTxErrorHandler $ do
      Q.unitQ "UPDATE hdb_catalog.hdb_table SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_relationship SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_permission SET is_system_defined = 'true'" () False
      Q.unitQ "UPDATE hdb_catalog.hdb_query_template SET is_system_defined = 'true'" () False

    isExtInstalled :: T.Text -> Q.Tx Bool
    isExtInstalled sn =
      (runIdentity . Q.getRow) <$> Q.withQ [Q.sql|
           SELECT EXISTS (
               SELECT 1
                 FROM pg_catalog.pg_available_extensions
                WHERE name = $1
           )
                    |] (Identity sn) False


cleanCatalog :: Q.TxE QErr ()
cleanCatalog = Q.catchE defaultTxErrorHandler $ do
  -- This is where the generated views and triggers are stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_views CASCADE" () False
  Q.unitQ "DROP SCHEMA hdb_catalog CASCADE" () False


execQuery :: BL.ByteString -> Q.TxE QErr BL.ByteString
execQuery queryBs = do
  query <- case A.decode queryBs of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"
  schemaCache <- buildSchemaCache
  tx <- liftEither $ buildTxAny adminUserInfo schemaCache query
  fst <$> tx
