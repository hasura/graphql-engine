module Hasura.RQL.DDL.QueryCollection
  ( runCreateCollection
  , runDropCollection
  , addCollectionP2
  , addCollectionToCatalog
  , delCollectionFromCatalog
  , runAddQueryToCollection
  , runDropQueryFromCollection
  , addToAllowlistSetup
  , runAddCollectionsToAllowlist
  , runDropCollectionsFromAllowlist
  , addCollectionsToAllowlistCatalog
  , delCollectionsFromAllowlistCatalog
  , fetchAllCollections
  , fetchAllowlist
  , module Hasura.RQL.Types.QueryCollection
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection
import           Hasura.Server.Utils              (duplicates)
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict              as HM
import qualified Data.Text                        as T
import qualified Data.Text.Extended               as T
import qualified Database.PG.Query                as Q

addCollectionP1
  :: (QErrM m, CacheRM m, UserInfoM m)
  => CollectionName -> m ()
addCollectionP1 name = do
  adminOnly
  collectionMap <- scQueryCollections <$> askSchemaCache
  withPathK "name" $
    case HM.lookup name collectionMap of
      Just _  -> throw400 AlreadyExists $
                 "query collection with name " <> name <<> " already exists"
      Nothing -> return ()

addCollectionP2
  :: (QErrM m, CacheRWM m)
  => CreateCollection -> m ()
addCollectionP2 (CreateCollection name defn _) =
  withPathK "queries" $ do
    unless (null duplicateNames) $ throw400 NotSupported $
      "found duplicate query names "
      <> T.intercalate ", " (map (T.dquote . unQueryName) duplicateNames)
    addCollectionToCache name queryList
  where
    CollectionDef queryList = defn
    duplicateNames = duplicates $ map _lqName queryList

runCreateCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => CreateCollection -> m EncJSON
runCreateCollection cc = do
  addCollectionP1 $ _ccName cc
  withPathK "definition" $ addCollectionP2 cc
  liftTx $ addCollectionToCatalog cc
  return successMsg

dropCollectionP1
  :: (QErrM m, CacheRM m, UserInfoM m)
  => CollectionName -> m ()
dropCollectionP1 name = do
  adminOnly
  collectionMap <- scQueryCollections <$> askSchemaCache
  withPathK "name" $
    case HM.lookup name collectionMap of
      Nothing -> throw400 NotExists $
                 "query collection with name " <> name <<> " does not exists"
      Just _ -> return ()

runAddQueryToCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => AddQueryToCollection -> m EncJSON
runAddQueryToCollection (AddQueryToCollection collName queryName query) = do
  adminOnly
  qMap <- addQueryToCollectionInCache collName queryName query
  let collDef = CollectionDef $ map (uncurry ListedQuery) $ HM.toList qMap
  liftTx $ addQueryToCollectionCatalog collName collDef
  return successMsg

runDropCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => DropCollection -> m EncJSON
runDropCollection (DropCollection name) = do
  dropCollectionP1 name
  delCollectionFromCache name
  delCollectionsFromAllowlist $ pure name
  liftTx $ delCollectionsFromAllowlistCatalog $ pure name
  liftTx $ delCollectionFromCatalog name
  return successMsg

runDropQueryFromCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => DropQueryFromCollection -> m EncJSON
runDropQueryFromCollection (DropQueryFromCollection collName queryName) = do
  adminOnly
  qMap <- dropQueryFromCollectionCache collName queryName
  let collDef = CollectionDef $ map (uncurry ListedQuery) $ HM.toList qMap
  liftTx $ delQueryFromCollectionCatalog collName collDef
  return successMsg

reportCollections :: [CollectionName] -> T.Text
reportCollections =
  T.intercalate ", " . map (T.dquote . unCollectionName)

handleDuplicateCollections :: QErrM m => [CollectionName] -> m ()
handleDuplicateCollections collNames =
  -- check for duplicates
  unless (null duplicateColls) $ throw400 NotSupported $
    "found duplicate collections; " <> reportCollections duplicateColls
  where
    duplicateColls = duplicates collNames

addToAllowlistSetup :: (QErrM m, CacheRWM m) => [CollectionName] -> m ()
addToAllowlistSetup collNames = do
  handleDuplicateCollections collNames
  sc <- askSchemaCache
  let allowlist = scAllowlist sc
      collsCurrList = flip filter collNames $
                      \c -> c `elem` map _qcName allowlist
  -- check for collections already present on current allow list
  unless (null collsCurrList) $ throw400 NotSupported $
    "collections already added to allow list " <> reportCollections collsCurrList

  colls <- indexedForM collNames $ \c -> do
    qMap <- askQueryMap c
    return $ QueryCollection c $ flip map (HM.toList qMap) $
      \(qn, q) -> ListedQuery qn $ queryWithoutTypeNames q

  addCollectionsToAllowlist colls

runAddCollectionsToAllowlist
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => CollectionsReq -> m EncJSON
runAddCollectionsToAllowlist (CollectionsReq collNames) = do
  adminOnly
  withPathK "collections" $ addToAllowlistSetup collNames
  liftTx $ addCollectionsToAllowlistCatalog collNames
  return successMsg

runDropCollectionsFromAllowlist
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => CollectionsReq -> m EncJSON
runDropCollectionsFromAllowlist (CollectionsReq collNames) = do
  adminOnly
  liftTx $ delCollectionsFromAllowlistCatalog collNames
  withPathK "collections" $ do
    handleDuplicateCollections collNames
    allowlist <- scAllowlist <$> askSchemaCache
    let invalidCollNames = collNames \\ map _qcName allowlist
    unless (null invalidCollNames) $ throw400 NotFound $
      "collections are not found in allow list; "
      <> reportCollections invalidCollNames
    delCollectionsFromAllowlist collNames
  return successMsg

-- Database functions
fetchAllCollections :: Q.TxE QErr [CreateCollection]
fetchAllCollections = do
  r <- Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT collection_name, collection_defn::json, comment
             FROM hdb_catalog.hdb_query_collection
          |] () False
  return $ flip map r $ \(name, Q.AltJ defn, mComment)
                        -> CreateCollection name defn mComment

fetchAllowlist :: Q.TxE QErr [CollectionName]
fetchAllowlist = map runIdentity <$>
  Q.listQE defaultTxErrorHandler [Q.sql|
      SELECT collection_name
        FROM hdb_catalog.hdb_allowlist
     |] () True

addCollectionToCatalog :: CreateCollection -> Q.TxE QErr ()
addCollectionToCatalog (CreateCollection name defn mComment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT INTO hdb_catalog.hdb_query_collection
      (collection_name, collection_defn, comment)
    VALUES ($1, $2, $3)
  |] (name, Q.AltJ defn, mComment) True

delCollectionFromCatalog :: CollectionName -> Q.TxE QErr ()
delCollectionFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
     DELETE FROM hdb_catalog.hdb_query_collection
     WHERE collection_name = $1
  |] (Identity name) True

addQueryToCollectionCatalog
  :: CollectionName -> CollectionDef -> Q.TxE QErr ()
addQueryToCollectionCatalog collName def = do
  -- Update definition
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_query_collection
       SET collection_defn = $1
     WHERE collection_name = $2
  |] (Q.AltJ def, collName) True

delQueryFromCollectionCatalog
  :: CollectionName -> CollectionDef -> Q.TxE QErr ()
delQueryFromCollectionCatalog collName def = do
  -- Update definition
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_query_collection
       SET collection_defn = $1
     WHERE collection_name = $2
  |] (Q.AltJ def, collName) True

encCollNamesToArray :: [CollectionName] -> Text
encCollNamesToArray colls =
  "{" <> T.intercalate "," (map unCollectionName colls) <> "}"

addCollectionsToAllowlistCatalog :: [CollectionName] -> Q.TxE QErr ()
addCollectionsToAllowlistCatalog collNames =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      INSERT INTO hdb_catalog.hdb_allowlist
                   (collection_name)
            VALUES (unnest($1::text[]))
      |] (Identity $ encCollNamesToArray collNames) True

delCollectionsFromAllowlistCatalog :: [CollectionName] -> Q.TxE QErr ()
delCollectionsFromAllowlistCatalog collNames =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_allowlist
         WHERE collection_name IN
         (SELECT q.name FROM unnest($1::text[]) as q(name))
      |] (Identity $ encCollNamesToArray collNames) True
