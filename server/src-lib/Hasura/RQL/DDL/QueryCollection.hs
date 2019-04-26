module Hasura.RQL.DDL.QueryCollection
  ( runCreateCollection
  , runDropCollection
  , addCollectionP2
  , addCollectionToCatalog
  , delCollectionFromCatalog
  , fetchAllCollections
  , runAddQueryToCollection
  , runDropQueryFromCollection
  , module Hasura.RQL.Types.QueryCollection
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection
import           Hasura.Server.Utils              (duplicates)
import           Hasura.SQL.Types

import qualified Data.Aeson                       as J
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
    queryList <- indexedForM unparsedQueries toParsedWhitelistedQuery
    let duplicateNames = duplicates $ map _wlqName queryList
    unless (null duplicateNames) $ throw400 NotSupported $
      "found duplicate query names "
      <> T.intercalate ", " (map (T.dquote . unQueryName) duplicateNames)
    addCollectionToCache name queryList
  where
    CollectionDef unparsedQueries = defn

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
runAddQueryToCollection (AddQueryToCollection collName queryName qText) = do
  adminOnly
  WhitelistedQuery _ gqlQuery <- toParsedWhitelistedQuery unparsedQuery
  addQueryToCollectionInCache collName queryName gqlQuery
  liftTx $ addQueryToCollectionCatalog collName unparsedQuery
  return successMsg
  where
    unparsedQuery = WhitelistedQuery queryName $ GQLQuery qText

runDropCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => DropCollection -> m EncJSON
runDropCollection (DropCollection name) = do
  dropCollectionP1 name
  delCollectionFromCache name
  liftTx $ delCollectionFromCatalog name
  return successMsg

runDropQueryFromCollection
  :: (QErrM m, UserInfoM m, MonadTx m, CacheRWM m)
  => DropQueryFromCollection -> m EncJSON
runDropQueryFromCollection (DropQueryFromCollection collName queryName) = do
  adminOnly
  dropQueryFromCollectionCache collName queryName
  liftTx $ delQueryFromCollectionCatalog collName queryName
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

fetchCollectionDef :: CollectionName -> Q.TxE QErr (CollectionDef T.Text)
fetchCollectionDef collName =
  (Q.getAltJ . runIdentity . Q.getRow) <$>
     Q.withQE defaultTxErrorHandler [Q.sql|
       SELECT collection_defn::json
         FROM hdb_catalog.hdb_query_collection
        WHERE collection_name = $1
     |] (Identity collName) True

addQueryToCollectionCatalog
  :: CollectionName -> WhitelistedQuery T.Text -> Q.TxE QErr ()
addQueryToCollectionCatalog collName query = do
  -- Fetch definition from catalog
  CollectionDef unparsedQueries <- fetchCollectionDef collName

  let newDef = J.toJSON $ CollectionDef $ unparsedQueries <> pure query

  -- Update definition
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_query_collection
       SET collection_defn = $1
     WHERE collection_name = $2
  |] (Q.AltJ newDef, collName) True

delQueryFromCollectionCatalog
  :: CollectionName -> QueryName -> Q.TxE QErr ()
delQueryFromCollectionCatalog collName queryName = do
  -- Fetch definition from catalog
  CollectionDef unparsedQueries <- fetchCollectionDef collName

  let newDef = J.toJSON $ CollectionDef $
               flip filter unparsedQueries $ \q -> _wlqName q /= queryName

  -- Update definition
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_query_collection
       SET collection_defn = $1
     WHERE collection_name = $2
  |] (Q.AltJ newDef, collName) True
