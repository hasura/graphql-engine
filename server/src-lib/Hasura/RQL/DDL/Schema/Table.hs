-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable(..)
  , runTrackTableQ
  , trackExistingTableOrViewP2

  , UntrackTable(..)
  , runUntrackTableQ

  , SetTableIsEnum(..)
  , runSetExistingTableIsEnumQ

  , buildTableCache
  , delTableAndDirectDeps
  , processTableChanges
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission
import {-# SOURCE #-} Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Enum
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Schema         as GS

import           Control.Lens.Extended         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)
import           Network.URI.Extended          ()

import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T

data TrackTable
  = TrackTable
  { tName   :: !QualifiedTable
  , tIsEnum :: !Bool
  } deriving (Show, Eq, Lift)

instance FromJSON TrackTable where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable <$> parseJSON v <*> pure False

instance ToJSON TrackTable where
  toJSON (TrackTable name isEnum)
    | isEnum = object [ "table" .= name, "is_enum" .= isEnum ]
    | otherwise = toJSON name

data SetTableIsEnum
  = SetTableIsEnum
  { stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''SetTableIsEnum)

data UntrackTable =
  UntrackTable
  { utTable   :: !QualifiedTable
  , utCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1
  :: (CacheRM m, UserInfoM m, QErrM m) => TrackTable -> m ()
trackExistingTableOrViewP1 TrackTable { tName = vn } = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member vn $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> vn
  let qf = fmap (FunctionName . getTableTxt) vn
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> vn <<> " already exists"

trackExistingTableOrViewP2
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => TrackTable -> m EncJSON
trackExistingTableOrViewP2 (TrackTable tableName isEnum) = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
  GS.checkConflictingNode defGCtx $ GS.qualObjectToName tableName
  saveTableToCatalog tableName isEnum
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

runTrackTableQ
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => TrackTable -> m EncJSON
runTrackTableQ q = do
  trackExistingTableOrViewP1 q
  trackExistingTableOrViewP2 q

runSetExistingTableIsEnumQ
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum tableName isEnum) = do
  adminOnly
  void $ askTabInfo tableName -- assert that table is tracked
  updateTableIsEnumInCatalog tableName isEnum
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

unTrackExistingTableOrViewP1
  :: (CacheRM m, UserInfoM m, QErrM m) => UntrackTable -> m ()
unTrackExistingTableOrViewP1 (UntrackTable vn _) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  case M.lookup vn (scTables rawSchemaCache) of
    Just ti ->
      -- Check if table/view is system defined
      when (_tiSystemDefined ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable qtn cascade) = do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the dependents from state
  mapM_ purgeDependentObject indirectDeps

  -- delete the table and its direct dependencies
  delTableAndDirectDeps qtn

  return successMsg
  where
    isDirectDep = \case
      (SOTableObj dtn _) -> qtn == dtn
      _                  -> False

runUntrackTableQ
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

processTableChanges :: (MonadTx m, CacheRWM m)
                    => TableInfo PGColumnInfo -> TableDiff -> m Bool
processTableChanges ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tiName ti
      withOldTabName = do
        replaceConstraints tn
        procDroppedCols tn
        procAddedCols tn
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = GS.qualObjectToName newTN
            defGCtx = scDefaultRemoteGCtx sc
        -- check for GraphQL schema conflicts on new name
        GS.checkConflictingNode defGCtx tnGQL
        void $ procAlteredCols sc tn
        -- update new table in catalog
        renameTableInCatalog newTN tn
        return True

  maybe withOldTabName withNewTabName mNewName

  where
    TableDiff mNewName droppedCols addedCols alteredCols _ constraints = tableDiff
    replaceConstraints tn = flip modTableInCache tn $ \tInfo ->
      return $ tInfo {_tiUniqOrPrimConstraints = constraints}

    procDroppedCols tn =
      forM_ droppedCols $ \droppedCol ->
        -- Drop the column from the cache
        delColFromCache droppedCol tn

    procAddedCols tn =
      -- In the newly added columns check that there is no conflict with relationships
      forM_ addedCols $ \rawInfo@(PGRawColumnInfo colName _ _ _) ->
        case M.lookup (fromPGCol colName) $ _tiFieldInfoMap ti of
          Just (FIRelationship _) ->
            throw400 AlreadyExists $ "cannot add column " <> colName
            <<> " in table " <> tn <<>
            " as a relationship with the name already exists"
          _ -> do
            info <- processColumnInfoUsingCache tn rawInfo
            addColToCache colName info tn

    procAlteredCols sc tn = fmap or $ forM alteredCols $
      \( PGRawColumnInfo oldName oldType _ _
       , newRawInfo@(PGRawColumnInfo newName newType _ _) ) -> do
        let performColumnUpdate = do
              newInfo <- processColumnInfoUsingCache tn newRawInfo
              updColInCache newName newInfo tn

        if | oldName /= newName -> renameColInCatalog oldName newName tn ti $> True

           | oldType /= newType -> do
              let colId = SOTableObj tn $ TOCol oldName
                  typeDepObjs = getDependentObjsWith (== DROnType) sc colId

              unless (null typeDepObjs) $ throw400 DependencyError $
                "cannot change type of column " <> oldName <<> " in table "
                <> tn <<> " because of the following dependencies : " <>
                reportSchemaObjs typeDepObjs

              performColumnUpdate

              -- If any dependent permissions found with the column whose type being altered is
              -- provided with a session variable, then rebuild permission info and update the cache
              let sessVarDepObjs = getDependentObjsWith (== DRSessionVariable) sc colId
              forM_ sessVarDepObjs $ \case
                SOTableObj qt (TOPerm rn pt) -> rebuildPermInfo qt rn pt
                _ -> throw500 "unexpected schema dependency found for altering column type"
              pure False

           | otherwise -> performColumnUpdate $> False

delTableAndDirectDeps
  :: (QErrM m, CacheRWM m, MonadTx m) => QualifiedTable -> m ()
delTableAndDirectDeps qtn@(QualifiedObject sn tn) = do
  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_relationship"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_permission"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."event_triggers"
             WHERE schema_name = $1 AND table_name = $2
              |] (sn, tn) False
  deleteTableFromCatalog qtn
  delTableFromCache qtn

-- | Builds an initial @'TableCache' 'PGColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall m. (MonadTx m, CacheRWM m)
  => [CatalogTable] -> m (TableCache PGColumnInfo)
buildTableCache = processTableCache <=< buildRawTableCache
  where
    withTable name = withSchemaObject $
      InconsistentMetadataObj (MOTable name) MOTTable (toJSON name)

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableCache :: [CatalogTable] -> m (TableCache PGRawColumnInfo)
    buildRawTableCache catalogTables = fmap (M.fromList . catMaybes) . for catalogTables $
      \(CatalogTable name isSystemDefined isEnum maybeInfo) -> withTable name $ do
        catalogInfo <- onNothing maybeInfo $
          throw400 NotExists $ "no such table/view exists in postgres: " <>> name

        let CatalogTableInfo columns constraints primaryKeyColumnNames viewInfo = catalogInfo
            columnFields = M.fromList . flip map columns $ \column ->
              (fromPGCol $ prciName column, FIColumn column)

            primaryKeyColumns = flip filter columns $ \column ->
              prciName column `elem` primaryKeyColumnNames
            fetchEnumValues = fetchAndValidateEnumValues name primaryKeyColumns columns

        maybeEnumValues <- if isEnum then Just <$> fetchEnumValues else pure Nothing

        let info = TableInfo
              { _tiName = name
              , _tiSystemDefined = isSystemDefined
              , _tiFieldInfoMap = columnFields
              , _tiRolePermInfoMap = mempty
              , _tiUniqOrPrimConstraints = constraints
              , _tiPrimaryKeyCols = primaryKeyColumnNames
              , _tiViewInfo = viewInfo
              , _tiEventTriggerInfoMap = mempty
              , _tiEnumValues = maybeEnumValues }
        pure (name, info)

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableCache :: TableCache PGRawColumnInfo -> m (TableCache PGColumnInfo)
    processTableCache rawTables = fmap (M.mapMaybe id) . for rawTables $ \rawInfo -> do
      let tableName = _tiName rawInfo
      withTable tableName $ rawInfo
        & tiFieldInfoMap.traverse._FIColumn %%~ processColumnInfo enumTables tableName
      where
        enumTables = M.mapMaybe _tiEnumValues rawTables

-- | “Processes” a 'PGRawColumnInfo' into a 'PGColumnInfo' by resolving its type using a map of known
-- enum tables.
processColumnInfo
  :: (QErrM m)
  => M.HashMap QualifiedTable EnumValues -- ^ known enum tables
  -> QualifiedTable -- ^ the table this column belongs to
  -> PGRawColumnInfo -- ^ the column’s raw information
  -> m PGColumnInfo
processColumnInfo enumTables tableName rawInfo = do
  resolvedType <- resolveColumnType
  pure PGColumnInfo
    { pgiName = prciName rawInfo
    , pgiType = resolvedType
    , pgiIsNullable = prciIsNullable rawInfo }
  where
    resolveColumnType =
      case prciReferences rawInfo of
        -- no referenced tables? definitely not an enum
        [] -> pure $ PGColumnScalar (prciType rawInfo)

        -- one referenced table? might be an enum, so check if the referenced table is an enum
        [referencedTableName] -> pure $ M.lookup referencedTableName enumTables & maybe
          (PGColumnScalar $ prciType rawInfo)
          (PGColumnEnumReference . EnumReference referencedTableName)

        -- multiple referenced tables? we could check if any of them are enums, but the schema is
        -- strange, so let’s just reject it
        referencedTables -> throw400 ConstraintViolation
          $ "cannot handle exotic schema: column " <> prciName rawInfo <<> " in table "
          <> tableName <<> " references multiple foreign tables ("
          <> T.intercalate ", " (map dquote referencedTables) <> ")?"

-- | Like 'processColumnInfo', but uses the information in the current schema cache to resolve a
-- column’s type.
processColumnInfoUsingCache :: (CacheRM m, QErrM m) => QualifiedTable -> PGRawColumnInfo -> m PGColumnInfo
processColumnInfoUsingCache tableName rawInfo = do
  tables <- scTables <$> askSchemaCache
  processColumnInfo (M.mapMaybe _tiEnumValues tables) tableName rawInfo
