module Hasura.RQL.DDL.QueryCollection
  ( runCreateCollection
  , runDropCollection
  -- , addCollectionToCatalog
  -- , delCollectionFromCatalog
  , runAddQueryToCollection
  , runDropQueryFromCollection
  , runAddCollectionToAllowlist
  , runDropCollectionFromAllowlist
  -- , addCollectionToAllowlistCatalog
  -- , delCollectionFromAllowlistCatalog
  , fetchAllCollections
  , fetchAllowlist
  , module Hasura.RQL.Types.QueryCollection
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import           Hasura.RQL.DDL.Schema.Catalog    (fetchMetadata)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection
import           Hasura.SQL.Types

import           Control.Lens                     (ix)
import           Data.List.Extended               (duplicates)

import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.Text                        as T
import qualified Data.Text.Extended               as T

addCollectionP2
  :: (QErrM m)
  => CollectionDef -> m ()
addCollectionP2 (CollectionDef queryList) =
  withPathK "queries" $
    unless (null duplicateNames) $ throw400 NotSupported $
      "found duplicate query names "
      <> T.intercalate ", " (map (T.dquote . unNonEmptyText . unQueryName) $ toList duplicateNames)
  where
    duplicateNames = duplicates $ map _lqName queryList

runCreateCollection
  :: (QErrM m, CacheRWM m, MonadTx m, HasSystemDefined m)
  => CreateCollection -> m EncJSON
runCreateCollection cc = do
  collDetM <- getCollectionDefM collName
  withPathK "name" $
    onJust collDetM $ const $ throw400 AlreadyExists $
      "query collection with name " <> collName <<> " already exists"
  withPathK "definition" $ addCollectionP2 def
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections %~ HM.insert collName cc
  return successMsg
  where
    CreateCollection collName def _ = cc

runAddQueryToCollection
  :: (CacheRWM m, MonadTx m)
  => AddQueryToCollection -> m EncJSON
runAddQueryToCollection (AddQueryToCollection collName queryName query) = do
  (CreateCollection _ (CollectionDef qList) comment) <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName

  when queryExists $ throw400 AlreadyExists $ "query with name "
    <> queryName <<> " already exists in collection " <>> collName
  let collDef = CollectionDef $ qList <> pure listQ
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections
      %~ HM.insert collName (CreateCollection collName collDef comment)
  return successMsg
  where
    listQ = ListedQuery queryName query

runDropCollection
  :: (MonadTx m, CacheRWM m)
  => DropCollection -> m EncJSON
runDropCollection (DropCollection collName cascade) = do
  allowlistModifier <- withPathK "collection" $ do
    -- FIXME? Optimise below code, maybe one query?
    -- check for query collection
    void $ getCollectionDef collName

    allowlist <- liftTx fetchAllowlist
    if collName `elem` allowlist && not cascade then
        throw400 DependencyError $ "query collection with name "
          <> collName <<> " is present in allowlist; cannot proceed to drop"
      else
        -- drop collection in allowlist
        pure $ metaAllowlist %~ HS.delete (CollectionReq collName)

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ allowlistModifier . (metaQueryCollections %~ HM.delete collName)

  pure successMsg

runDropQueryFromCollection
  :: (CacheRWM m, MonadTx m)
  => DropQueryFromCollection -> m EncJSON
runDropQueryFromCollection (DropQueryFromCollection collName queryName) = do
  CreateCollection _ (CollectionDef qList) _ <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName
  when (not queryExists) $ throw400 NotFound $ "query with name "
    <> queryName <<> " not found in collection " <>> collName

  -- liftTx $ updateCollectionDefCatalog collName collDef
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections.ix collName.ccDefinition.cdQueries
      %~ filter ((/=) queryName . _lqName)
  pure successMsg

runAddCollectionToAllowlist
  :: (MonadTx m, CacheRWM m)
  => CollectionReq -> m EncJSON
runAddCollectionToAllowlist req@(CollectionReq collName) = do
  void $ withPathK "collection" $ getCollectionDef collName
  -- liftTx $ addCollectionToAllowlistCatalog collName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaAllowlist %~ HS.insert req
  pure successMsg

runDropCollectionFromAllowlist
  :: (UserInfoM m, MonadTx m, CacheRWM m)
  => CollectionReq -> m EncJSON
runDropCollectionFromAllowlist req@(CollectionReq collName) = do
  void $ withPathK "collection" $ getCollectionDef collName
  -- liftTx $ delCollectionFromAllowlistCatalog collName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaAllowlist %~ HS.delete req
  return successMsg

getCollectionDef
  :: (QErrM m, MonadTx m)
  => CollectionName -> m CreateCollection
getCollectionDef collName = do
  detM <- getCollectionDefM collName
  onNothing detM $ throw400 NotExists $
    "query collection with name " <> collName <<> " does not exists"

getCollectionDefM
  :: (QErrM m, MonadTx m)
  => CollectionName -> m (Maybe CreateCollection)
getCollectionDefM collName =
  HM.lookup collName <$> liftTx fetchAllCollections

fetchAllCollections :: MonadTx m => m QueryCollections
fetchAllCollections =
  _metaQueryCollections <$> fetchMetadata

fetchAllowlist :: MonadTx m => m [CollectionName]
fetchAllowlist =
  (map _crCollection . toList . _metaAllowlist) <$> fetchMetadata

-- Database functions
-- fetchAllCollections :: Q.TxE QErr [CreateCollection]
-- fetchAllCollections = do
--   r <- Q.listQE defaultTxErrorHandler [Q.sql|
--            SELECT collection_name, collection_defn::json, comment
--              FROM hdb_catalog.hdb_query_collection
--           |] () False
--   return $ flip map r $ \(name, Q.AltJ defn, mComment)
--                         -> CreateCollection name defn mComment

-- fetchAllowlist :: Q.TxE QErr [CollectionName]
-- fetchAllowlist = map runIdentity <$>
--   Q.listQE defaultTxErrorHandler [Q.sql|
--       SELECT collection_name
--         FROM hdb_catalog.hdb_allowlist
--      |] () True

-- addCollectionToCatalog :: CreateCollection -> SystemDefined -> Q.TxE QErr ()
-- addCollectionToCatalog (CreateCollection name defn mComment) systemDefined =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--     INSERT INTO hdb_catalog.hdb_query_collection
--       (collection_name, collection_defn, comment, is_system_defined)
--     VALUES ($1, $2, $3, $4)
--   |] (name, Q.AltJ defn, mComment, systemDefined) True

-- delCollectionFromCatalog :: CollectionName -> Q.TxE QErr ()
-- delCollectionFromCatalog name =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--      DELETE FROM hdb_catalog.hdb_query_collection
--      WHERE collection_name = $1
--   |] (Identity name) True

-- updateCollectionDefCatalog
--   :: CollectionName -> CollectionDef -> Q.TxE QErr ()
-- updateCollectionDefCatalog collName def = do
--   -- Update definition
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--     UPDATE hdb_catalog.hdb_query_collection
--        SET collection_defn = $1
--      WHERE collection_name = $2
--   |] (Q.AltJ def, collName) True

-- addCollectionToAllowlistCatalog :: CollectionName -> Q.TxE QErr ()
-- addCollectionToAllowlistCatalog collName =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--       INSERT INTO hdb_catalog.hdb_allowlist
--                    (collection_name)
--             VALUES ($1)
--       |] (Identity collName) True

-- delCollectionFromAllowlistCatalog :: CollectionName -> Q.TxE QErr ()
-- delCollectionFromAllowlistCatalog collName =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--       DELETE FROM hdb_catalog.hdb_allowlist
--          WHERE collection_name = $1
--       |] (Identity collName) True
