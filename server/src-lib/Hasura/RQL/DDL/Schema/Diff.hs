module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta (..),
    FunctionMeta (..),
    TablesDiff (..),
    FunctionsDiff (..),
    ComputedFieldMeta (..),
    getTablesDiff,
    processTablesDiff,
    getIndirectDependenciesFromTableDiff,
    getFunctionsDiff,
    getOverloadedFunctions,
  )
where

import Control.Lens ((.~))
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as HS
import Data.List.Extended
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types hiding (ConstraintName, FunctionName, TableName)
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Rename
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.API
import Hasura.Table.Cache
import Hasura.Table.Metadata (tmConfiguration)
import Language.GraphQL.Draft.Syntax qualified as G

data FunctionMeta b = FunctionMeta
  { fmOid :: OID,
    fmFunction :: FunctionName b,
    fmType :: FunctionVolatility
  }
  deriving (Generic)

deriving instance (Backend b) => Show (FunctionMeta b)

deriving instance (Backend b) => Eq (FunctionMeta b)

instance (Backend b) => FromJSON (FunctionMeta b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (FunctionMeta b) where
  toJSON = genericToJSON hasuraJSON

data ComputedFieldMeta b = ComputedFieldMeta
  { ccmName :: ComputedFieldName,
    ccmFunctionMeta :: FunctionMeta b
  }
  deriving (Generic, Show, Eq)

instance (Backend b) => FromJSON (ComputedFieldMeta b) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => ToJSON (ComputedFieldMeta b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

data TableMeta (b :: BackendType) = TableMeta
  { tmTable :: TableName b,
    tmInfo :: DBTableMetadata b,
    tmComputedFields :: [ComputedFieldMeta b]
  }
  deriving (Show, Eq)

data ComputedFieldDiff (b :: BackendType) = ComputedFieldDiff
  { _cfdDropped :: [ComputedFieldName],
    _cfdAltered :: [(ComputedFieldMeta b, ComputedFieldMeta b)],
    _cfdOverloaded :: [(ComputedFieldName, FunctionName b)]
  }

deriving instance (Backend b) => Show (ComputedFieldDiff b)

deriving instance (Backend b) => Eq (ComputedFieldDiff b)

data TableDiff (b :: BackendType) = TableDiff
  { _tdNewName :: Maybe (TableName b),
    _tdDroppedCols :: [Column b],
    _tdAlteredCols :: [(RawColumnInfo b, RawColumnInfo b)],
    _tdDroppedFKeyCons :: [ConstraintName b],
    _tdComputedFields :: ComputedFieldDiff b,
    -- The final list of uniq/primary constraint names
    -- used for generating types on_conflict clauses
    -- TODO: this ideally should't be part of TableDiff
    _tdUniqOrPriCons :: [ConstraintName b],
    _tdNewDescription :: Maybe PGDescription
  }

getTableDiff ::
  (Backend b) =>
  TableMeta b ->
  TableMeta b ->
  TableDiff b
getTableDiff oldtm newtm =
  TableDiff
    mNewName
    droppedCols
    alteredCols
    droppedFKeyConstraints
    computedFieldDiff
    uniqueOrPrimaryCons
    mNewDesc
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = _ptmiColumns $ tmInfo oldtm
    newCols = _ptmiColumns $ tmInfo newtm

    uniqueOrPrimaryCons =
      map _cName
        $ maybeToList (_pkConstraint <$> _ptmiPrimaryKey (tmInfo newtm))
        <> (_ucConstraint <$> toList (_ptmiUniqueConstraints (tmInfo newtm)))

    mNewDesc = _ptmiDescription $ tmInfo newtm

    droppedCols = map rciName $ getDifferenceOn rciPosition oldCols newCols
    existingCols = getOverlapWith rciPosition oldCols newCols
    alteredCols = filter (uncurry (/=)) existingCols

    -- foreign keys are considered dropped only if their oid
    -- and (ref-table, column mapping) are changed
    droppedFKeyConstraints =
      map (_cName . _fkConstraint)
        $ HS.toList
        $ droppedFKeysWithOid
        `HS.intersection` droppedFKeysWithUniq
    tmForeignKeys = fmap unForeignKeyMetadata . toList . _ptmiForeignKeys . tmInfo
    droppedFKeysWithOid =
      HS.fromList
        $ (getDifferenceOn (_cOid . _fkConstraint) `on` tmForeignKeys) oldtm newtm
    droppedFKeysWithUniq =
      HS.fromList
        $ (getDifferenceOn mkFKeyUniqId `on` tmForeignKeys) oldtm newtm
    mkFKeyUniqId (ForeignKey _ reftn colMap) = (reftn, colMap)

    -- calculate computed field diff
    oldComputedFieldMeta = tmComputedFields oldtm
    newComputedFieldMeta = tmComputedFields newtm

    droppedComputedFields =
      map ccmName
        $ getDifferenceOn (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    alteredComputedFields =
      getOverlapWith (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    overloadedComputedFieldFunctions =
      let getFunction = fmFunction . ccmFunctionMeta
          getSecondElement (_ NE.:| list) = listToMaybe list
       in mapMaybe (fmap ((&&&) ccmName getFunction) . getSecondElement)
            $ flip NE.groupBy newComputedFieldMeta
            $ \l r ->
              ccmName l == ccmName r && getFunction l == getFunction r

    computedFieldDiff =
      ComputedFieldDiff
        droppedComputedFields
        alteredComputedFields
        overloadedComputedFieldFunctions

getTableChangeDeps ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TableName b ->
  TableDiff b ->
  m [SchemaObjId]
getTableChangeDeps source tn tableDiff = do
  sc <- askSchemaCache
  -- for all the dropped columns
  droppedColDeps <- fmap concat
    $ forM droppedCols
    $ \droppedCol -> do
      let objId =
            SOSourceObj source
              $ AB.mkAnyBackend
              $ SOITableObj @b tn
              $ TOCol @b droppedCol
      return $ getDependentObjs sc objId
  -- for all dropped constraints
  droppedConsDeps <- fmap concat
    $ forM droppedFKeyConstraints
    $ \droppedCons -> do
      let objId =
            SOSourceObj source
              $ AB.mkAnyBackend
              $ SOITableObj @b tn
              $ TOForeignKey @b droppedCons
      return $ getDependentObjs sc objId
  return $ droppedConsDeps <> droppedColDeps <> droppedComputedFieldDeps
  where
    TableDiff _ droppedCols _ droppedFKeyConstraints computedFieldDiff _ _ = tableDiff
    droppedComputedFieldDeps =
      map
        ( SOSourceObj source
            . AB.mkAnyBackend
            . SOITableObj @b tn
            . TOComputedField
        )
        $ _cfdDropped computedFieldDiff

data TablesDiff (b :: BackendType) = TablesDiff
  { _sdDroppedTables :: [TableName b],
    _sdAlteredTables :: [(TableName b, TableDiff b)]
  }

getTablesDiff ::
  (Backend b) => [TableMeta b] -> [TableMeta b] -> TablesDiff b
getTablesDiff oldMeta newMeta =
  TablesDiff droppedTables survivingTables
  where
    droppedTables = map tmTable $ getDifferenceOn (_ptmiOid . tmInfo) oldMeta newMeta
    survivingTables =
      flip map (getOverlapWith (_ptmiOid . tmInfo) oldMeta newMeta) $ \(oldtm, newtm) ->
        (tmTable oldtm, getTableDiff oldtm newtm)

getIndirectDependenciesFromTableDiff ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TablesDiff b ->
  m [SchemaObjId]
getIndirectDependenciesFromTableDiff source tablesDiff = do
  sc <- askSchemaCache
  let tableIds = SOSourceObj source . AB.mkAnyBackend . SOITable @b <$> droppedTables
      tableDropDeps = concatMap (getDependentObjs sc) tableIds
  tableModDeps <- concat <$> traverse (uncurry (getTableChangeDeps source)) alteredTables
  pure $ filter isIndirectDep $ HS.toList $ HS.fromList $ tableDropDeps <> tableModDeps
  where
    TablesDiff droppedTables alteredTables = tablesDiff
    -- we keep all table objects that are not tied to a deleted table
    isIndirectDep :: SchemaObjId -> Bool
    isIndirectDep = \case
      -- table objects in the same source
      SOSourceObj s obj
        | s == source,
          Just (SOITableObj tn _) <- AB.unpackAnyBackend @b obj ->
            not $ tn `HS.member` HS.fromList droppedTables
      -- table objects in any other source
      SOSourceObj _ obj ->
        AB.runBackend obj \case
          SOITableObj {} -> True
          _ -> False
      -- any other kind of schema object
      _ -> False

data FunctionsDiff b = FunctionsDiff
  { fdDropped :: [FunctionName b],
    fdAltered :: [(FunctionName b, FunctionVolatility)]
  }

deriving instance (Backend b) => Show (FunctionsDiff b)

deriving instance (Backend b) => Eq (FunctionsDiff b)

getFunctionsDiff :: [FunctionMeta b] -> [FunctionMeta b] -> FunctionsDiff b
getFunctionsDiff oldMeta newMeta =
  FunctionsDiff droppedFuncs alteredFuncs
  where
    droppedFuncs = map fmFunction $ getDifferenceOn fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlapWith fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          alteredFunc = (fmFunction oldfm, fmType newfm)
       in bool Nothing (Just alteredFunc) isTypeAltered

getOverloadedFunctions ::
  (Backend b) => [FunctionName b] -> [FunctionMeta b] -> [FunctionName b]
getOverloadedFunctions trackedFuncs newFuncMeta =
  toList $ duplicates $ map fmFunction trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      fmFunction fm `elem` trackedFuncs

processTablesDiff ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b,
    Column b ~ ColumnPath b
  ) =>
  SourceName ->
  TableCache b ->
  TablesDiff b ->
  m ()
processTablesDiff source preActionTables tablesDiff = do
  -- Purge the dropped tables
  dropTablesInMetadata @b source droppedTables

  for_ alteredTables $ \(oldQtn, tableDiff) -> do
    ti <-
      onNothing
        (HashMap.lookup oldQtn preActionTables)
        (throw500 $ "old table metadata not found in cache: " <>> oldQtn)
    alterTableInMetadata source (_tiCoreInfo ti) tableDiff
  where
    TablesDiff droppedTables alteredTables = tablesDiff

alterTableInMetadata ::
  forall m b.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b,
    Column b ~ ColumnPath b
  ) =>
  SourceName ->
  TableCoreInfo b ->
  TableDiff b ->
  m ()
alterTableInMetadata source ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tciName ti
      withOldTabName = do
        alterColumnsInMetadata source alteredCols tableFields sc tn

      withNewTabName :: TableName b -> m ()
      withNewTabName newTN = do
        -- check for GraphQL schema conflicts on new name
        liftEither (tableGraphQLName @b newTN) >>= checkConflictingNode sc . G.unName
        alterColumnsInMetadata source alteredCols tableFields sc tn
        -- update new table in metadata
        renameTableInMetadata @b source newTN tn

  -- Process computed field diff
  processComputedFieldDiff tn
  -- Drop custom column names and comments for dropped columns
  removeDroppedColumnsFromMetadataField source droppedCols ti
  maybe withOldTabName withNewTabName mNewName
  where
    TableDiff mNewName droppedCols alteredCols _ computedFieldDiff _ _ = tableDiff
    tableFields = _tciFieldInfoMap ti

    processComputedFieldDiff :: TableName b -> m ()
    processComputedFieldDiff table = do
      let ComputedFieldDiff _ altered overloaded = computedFieldDiff
          getFunction = fmFunction . ccmFunctionMeta
      forM_ overloaded $ \(columnName, function) ->
        throw400 NotSupported
          $ "The function "
          <> function
          <<> " associated with computed field"
          <> columnName
          <<> " of table "
          <> table
          <<> " is being overloaded"
      forM_ altered $ \(old, new) ->
        if
          | (fmType . ccmFunctionMeta) new == FTVOLATILE ->
              throw400 NotSupported
                $ "The type of function "
                <> getFunction old
                <<> " associated with computed field "
                <> ccmName old
                <<> " of table "
                <> table
                <<> " is being altered to \"VOLATILE\""
          | otherwise -> pure ()

dropTablesInMetadata ::
  forall b m.
  ( MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  [TableName b] ->
  m ()
dropTablesInMetadata source droppedTables =
  forM_ droppedTables
    $ \tn -> tell $ MetadataModifier $ metaSources . ix source . toSourceMetadata . (smTables @b) %~ InsOrdHashMap.delete tn

alterColumnsInMetadata ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b,
    Column b ~ ColumnPath b
  ) =>
  SourceName ->
  [(RawColumnInfo b, RawColumnInfo b)] ->
  FieldInfoMap (FieldInfo b) ->
  SchemaCache ->
  TableName b ->
  m ()
alterColumnsInMetadata source alteredCols fields sc tn =
  for_ alteredCols
    $ \( RawColumnInfo {rciName = oldName, rciType = oldType},
         RawColumnInfo {rciName = newName, rciType = newType}
         ) -> do
        if
          | oldName /= newName ->
              renameColumnInMetadata oldName newName source tn fields
          | oldType /= newType -> do
              let colId =
                    SOSourceObj source
                      $ AB.mkAnyBackend
                      $ SOITableObj @b tn
                      $ TOCol @b oldName
                  typeDepObjs = getDependentObjsWith (== DROnType) sc colId

              unless (null typeDepObjs)
                $ throw400 DependencyError
                $ "cannot change type of column "
                <> oldName
                <<> " in table "
                <> tn
                <<> " because of the following dependencies: "
                <> reportSchemaObjs typeDepObjs
          | otherwise -> pure ()

removeDroppedColumnsFromMetadataField ::
  forall b m.
  (MonadWriter MetadataModifier m, BackendMetadata b) =>
  SourceName ->
  [Column b] ->
  TableCoreInfo b ->
  m ()
removeDroppedColumnsFromMetadataField source droppedCols tableInfo = do
  when (newColumnConfig /= originalColumnConfig)
    $ tell
    $ MetadataModifier
    $ tableMetadataSetter @b source tableName
    . tmConfiguration
    .~ newTableConfig
  where
    tableName = _tciName tableInfo
    originalTableConfig = _tciCustomConfig tableInfo
    originalColumnConfig = _tcColumnConfig originalTableConfig
    newColumnConfig = foldl' (flip HashMap.delete) originalColumnConfig droppedCols
    newTableConfig = originalTableConfig & tcColumnConfig .~ newColumnConfig
