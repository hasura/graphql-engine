module Hasura.RQL.DDL.QueryCollection
  ( runCreateCollection,
    runRenameCollection,
    runDropCollection,
    runAddQueryToCollection,
    runDropQueryFromCollection,
    runAddCollectionToAllowlist,
    runDropCollectionFromAllowlist,
    runUpdateScopeOfCollectionInAllowlist,
  )
where

import Control.Lens ((.~))
import Data.Aeson qualified as J
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.Extended (duplicates)
import Data.Text.Extended
import Data.Text.NonEmpty
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.SchemaCache.Build

addCollectionP2 ::
  (QErrM m) =>
  CollectionDef ->
  m ()
addCollectionP2 (CollectionDef queryList) =
  withPathK "queries"
    $ unless (null duplicateNames)
    $ throw400 NotSupported
    $ "found duplicate query names "
    <> dquoteList (unNonEmptyText . unQueryName <$> toList duplicateNames)
  where
    duplicateNames = duplicates $ map _lqName queryList

runCreateCollection ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  CreateCollection ->
  m EncJSON
runCreateCollection cc = do
  collDetM <- getCollectionDefM collName
  withPathK "name"
    $ for_ collDetM
    $ const
    $ throw400 AlreadyExists
    $ "query collection with name "
    <> collName
    <<> " already exists"
  withPathK "definition" $ addCollectionP2 def
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections
    %~ InsOrdHashMap.insert collName cc
  return successMsg
  where
    CreateCollection collName def _ = cc

runRenameCollection ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  RenameCollection ->
  m EncJSON
runRenameCollection (RenameCollection oldName newName) = do
  _ <- getCollectionDef oldName
  newCollDefM <- getCollectionDefM newName
  withPathK "new_name"
    $ for_ newCollDefM
    $ const
    $ throw400 AlreadyExists
    $ "query collection with name "
    <> newName
    <<> " already exists"
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections
    %~ changeCollectionName oldName newName
  return successMsg
  where
    changeCollectionName :: CollectionName -> CollectionName -> QueryCollections -> QueryCollections
    changeCollectionName oldKey newKey oMap = case InsOrdHashMap.lookup oldKey oMap of
      Nothing -> oMap
      Just oldVal ->
        let newVal = oldVal & ccName .~ newKey
         in InsOrdHashMap.insert newKey newVal (InsOrdHashMap.delete oldKey oMap)

runAddQueryToCollection ::
  (CacheRWM m, MonadError QErr m, MetadataM m) =>
  AddQueryToCollection ->
  m EncJSON
runAddQueryToCollection (AddQueryToCollection collName queryName query) = do
  (CreateCollection _ (CollectionDef qList) comment) <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName

  when queryExists
    $ throw400 AlreadyExists
    $ "query with name "
    <> queryName
    <<> " already exists in collection "
    <>> collName
  let collDef = CollectionDef $ qList <> pure listQ
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections
    %~ InsOrdHashMap.insert collName (CreateCollection collName collDef comment)
  return successMsg
  where
    listQ = ListedQuery queryName query

runDropCollection ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  DropCollection ->
  m EncJSON
runDropCollection (DropCollection collName cascade) = do
  cascadeModifier <- withPathK "collection" $ do
    assertCollectionDefined collName
    allowlist <- fetchAllAllowlistCollections
    if (collName `elem` allowlist)
      then
        if not cascade
          then
            throw400 DependencyError
              $ "query collection with name "
              <> collName
              <<> " is present in the allowlist; cannot proceed to drop. "
              <> "please use cascade to confirm you wish to drop it from the allowlist as well"
          else dropCollectionFromAllowlist collName
      else pure mempty

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ cascadeModifier
    <> MetadataModifier (metaQueryCollections %~ InsOrdHashMap.delete collName)

  pure successMsg

runDropQueryFromCollection ::
  (CacheRWM m, MonadError QErr m, MetadataM m) =>
  DropQueryFromCollection ->
  m EncJSON
runDropQueryFromCollection (DropQueryFromCollection collName queryName) = do
  CreateCollection _ (CollectionDef qList) _ <- getCollectionDef collName
  let queryExists = flip any qList $ \q -> _lqName q == queryName
  unless queryExists
    $ throw400 NotFound
    $ "query with name "
    <> queryName
    <<> " not found in collection "
    <>> collName

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaQueryCollections
    . ix collName
    . ccDefinition
    . cdQueries
    %~ filter ((/=) queryName . _lqName)
  pure successMsg

runAddCollectionToAllowlist ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  AllowlistEntry ->
  m EncJSON
runAddCollectionToAllowlist entry = do
  withPathK "collection" $ assertCollectionDefined (aeCollection entry)
  allowlist <- withPathK "allowlist" fetchAllowlist
  case metadataAllowlistInsert entry allowlist of
    Left msg ->
      pure
        . encJFromJValue
        . J.object
        $ ["message" J..= msg]
    Right allowlist' -> do
      withNewInconsistentObjsCheck . buildSchemaCache $ MetadataModifier (metaAllowlist .~ allowlist')
      pure successMsg

-- Create a metadata modifier that drops a collection from the allowlist.
-- This is factored out for use in 'runDropCollection'.
dropCollectionFromAllowlist ::
  (MonadError QErr m, MetadataM m) =>
  CollectionName ->
  m MetadataModifier
dropCollectionFromAllowlist collName = do
  withPathK "collection" $ assertCollectionDefined collName
  allowList <- withPathK "allowlist" fetchAllowlist
  case InsOrdHashMap.lookup collName allowList of
    Nothing -> throw400 NotFound $ "collection " <> collName <<> " doesn't exist in the allowlist"
    Just _ -> pure $ MetadataModifier $ metaAllowlist .~ InsOrdHashMap.delete collName allowList

runDropCollectionFromAllowlist ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  DropCollectionFromAllowlist ->
  m EncJSON
runDropCollectionFromAllowlist (DropCollectionFromAllowlist collName) = do
  withNewInconsistentObjsCheck . buildSchemaCache =<< dropCollectionFromAllowlist collName
  return successMsg

runUpdateScopeOfCollectionInAllowlist ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  UpdateScopeOfCollectionInAllowlist ->
  m EncJSON
runUpdateScopeOfCollectionInAllowlist (UpdateScopeOfCollectionInAllowlist entry) = do
  withPathK "collection" $ assertCollectionDefined (aeCollection entry)
  al <- withPathK "allowlist" fetchAllowlist
  modifier <- case metadataAllowlistUpdateScope entry al of
    Left err -> throw400 NotFound err
    Right al' ->
      pure
        . MetadataModifier
        $ metaAllowlist
        .~ al'
  withNewInconsistentObjsCheck $ buildSchemaCache modifier
  return successMsg

-- helpers

assertCollectionDefined :: (QErrM m, MetadataM m) => CollectionName -> m ()
assertCollectionDefined = void . getCollectionDef

getCollectionDef ::
  (QErrM m, MetadataM m) =>
  CollectionName ->
  m CreateCollection
getCollectionDef collName = do
  detM <- getCollectionDefM collName
  onNothing detM
    $ throw400 NotExists
    $ "query collection with name "
    <> collName
    <<> " does not exist"

getCollectionDefM ::
  (QErrM m, MetadataM m) =>
  CollectionName ->
  m (Maybe CreateCollection)
getCollectionDefM collName =
  InsOrdHashMap.lookup collName <$> fetchAllCollections

fetchAllCollections :: (MetadataM m) => m QueryCollections
fetchAllCollections =
  _metaQueryCollections <$> getMetadata

fetchAllowlist :: (MetadataM m) => m MetadataAllowlist
fetchAllowlist = _metaAllowlist <$> getMetadata

fetchAllAllowlistCollections :: (MetadataM m) => m [CollectionName]
fetchAllAllowlistCollections = metadataAllowlistAllCollections <$> fetchAllowlist
