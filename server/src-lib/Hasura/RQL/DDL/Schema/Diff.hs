module Hasura.RQL.DDL.Schema.Diff where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as M
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.HashSet                       as HS
import qualified Data.List.NonEmpty                 as NE
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Lens                       ((.~))
import           Data.Aeson
import           Data.List.Extended
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.Backends.Postgres.SQL.Types hiding (ConstraintName, FunctionName, TableName)
import           Hasura.Base.Error
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types                   hiding (fmFunction, tmComputedFields, tmTable)


data FunctionMeta b
  = FunctionMeta
  { fmOid      :: !OID
  , fmFunction :: !(FunctionName b)
  , fmType     :: !FunctionVolatility
  } deriving (Generic)
deriving instance (Backend b) => Show (FunctionMeta b)
deriving instance (Backend b) => Eq (FunctionMeta b)

instance (Backend b) => FromJSON (FunctionMeta b) where
  parseJSON = genericParseJSON hasuraJSON
instance (Backend b) => ToJSON (FunctionMeta b) where
  toJSON = genericToJSON hasuraJSON

data ComputedFieldMeta b
  = ComputedFieldMeta
  { ccmName         :: !ComputedFieldName
  , ccmFunctionMeta :: !(FunctionMeta b)
  } deriving (Generic, Show, Eq)

instance (Backend b) => FromJSON (ComputedFieldMeta b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields=True}
instance (Backend b) => ToJSON (ComputedFieldMeta b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

data TableMeta (b :: BackendType)
  = TableMeta
  { tmTable          :: !(TableName b)
  , tmInfo           :: !(DBTableMetadata b)
  , tmComputedFields :: ![ComputedFieldMeta b]
  } deriving (Show, Eq)

data ComputedFieldDiff (b :: BackendType)
  = ComputedFieldDiff
  { _cfdDropped    :: [ComputedFieldName]
  , _cfdAltered    :: [(ComputedFieldMeta b, ComputedFieldMeta b)]
  , _cfdOverloaded :: [(ComputedFieldName, FunctionName b)]
  }
deriving instance (Backend b) => Show (ComputedFieldDiff b)
deriving instance (Backend b) => Eq (ComputedFieldDiff b)

data TableDiff (b :: BackendType)
  = TableDiff
  { _tdNewName         :: !(Maybe (TableName b))
  , _tdDroppedCols     :: ![Column b]
  , _tdAlteredCols     :: ![(RawColumnInfo b, RawColumnInfo b)]
  , _tdDroppedFKeyCons :: ![ConstraintName b]
  , _tdComputedFields  :: !(ComputedFieldDiff b)
  -- The final list of uniq/primary constraint names
  -- used for generating types on_conflict clauses
  -- TODO: this ideally should't be part of TableDiff
  , _tdUniqOrPriCons   :: ![ConstraintName b]
  , _tdNewDescription  :: !(Maybe PGDescription)
  }

getTableDiff
  :: Backend b
  => TableMeta b
  -> TableMeta b
  -> TableDiff b
getTableDiff oldtm newtm =
  TableDiff mNewName droppedCols alteredCols
  droppedFKeyConstraints computedFieldDiff uniqueOrPrimaryCons mNewDesc
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = _ptmiColumns $ tmInfo oldtm
    newCols = _ptmiColumns $ tmInfo newtm

    uniqueOrPrimaryCons = map _cName $
      maybeToList (_pkConstraint <$> _ptmiPrimaryKey (tmInfo newtm))
        <> toList (_ptmiUniqueConstraints $ tmInfo newtm)

    mNewDesc = _ptmiDescription $ tmInfo newtm

    droppedCols = map prciName $ getDifferenceOn prciPosition oldCols newCols
    existingCols = getOverlapWith prciPosition oldCols newCols
    alteredCols = filter (uncurry (/=)) existingCols

    -- foreign keys are considered dropped only if their oid
    -- and (ref-table, column mapping) are changed
    droppedFKeyConstraints = map (_cName . _fkConstraint) $ HS.toList $
      droppedFKeysWithOid `HS.intersection` droppedFKeysWithUniq
    tmForeignKeys = fmap unForeignKeyMetadata . toList . _ptmiForeignKeys . tmInfo
    droppedFKeysWithOid = HS.fromList $
      (getDifferenceOn (_cOid . _fkConstraint) `on` tmForeignKeys) oldtm newtm
    droppedFKeysWithUniq = HS.fromList $
      (getDifferenceOn mkFKeyUniqId `on` tmForeignKeys) oldtm newtm
    mkFKeyUniqId (ForeignKey _ reftn colMap) = (reftn, colMap)

    -- calculate computed field diff
    oldComputedFieldMeta = tmComputedFields oldtm
    newComputedFieldMeta = tmComputedFields newtm

    droppedComputedFields = map ccmName $
      getDifferenceOn (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    alteredComputedFields =
      getOverlapWith (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    overloadedComputedFieldFunctions =
      let getFunction = fmFunction . ccmFunctionMeta
          getSecondElement (_ NE.:| list) = listToMaybe list
      in mapMaybe (fmap ((&&&) ccmName getFunction) . getSecondElement) $
         flip NE.groupBy newComputedFieldMeta $ \l r ->
         ccmName l == ccmName r && getFunction l == getFunction r

    computedFieldDiff = ComputedFieldDiff droppedComputedFields alteredComputedFields
                      overloadedComputedFieldFunctions

getTableChangeDeps
  :: forall b m
   . (QErrM m, CacheRM m, Backend b)
  => SourceName
  -> TableName b
  -> TableDiff b
  -> m [SchemaObjId]
getTableChangeDeps source tn tableDiff = do
  sc <- askSchemaCache
  -- for all the dropped columns
  droppedColDeps <- fmap concat $ forM droppedCols $ \droppedCol -> do
    let objId = SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b tn
                  $ TOCol @b droppedCol
    return $ getDependentObjs sc objId
  -- for all dropped constraints
  droppedConsDeps <- fmap concat $ forM droppedFKeyConstraints $ \droppedCons -> do
    let objId = SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b tn
                  $ TOForeignKey @b droppedCons
    return $ getDependentObjs sc objId
  return $ droppedConsDeps <> droppedColDeps <> droppedComputedFieldDeps
  where
    TableDiff _ droppedCols _ droppedFKeyConstraints computedFieldDiff _ _ = tableDiff
    droppedComputedFieldDeps =
      map
        (SOSourceObj source
          . AB.mkAnyBackend
          . SOITableObj @b tn
          . TOComputedField)
        $ _cfdDropped computedFieldDiff

data SchemaDiff (b :: BackendType)
  = SchemaDiff
  { _sdDroppedTables :: ![TableName b]
  , _sdAlteredTables :: ![(TableName b, TableDiff b)]
  }

getSchemaDiff
  :: (Backend b) => [TableMeta b] -> [TableMeta b] -> SchemaDiff b
getSchemaDiff oldMeta newMeta =
  SchemaDiff droppedTables survivingTables
  where
    droppedTables = map tmTable $ getDifferenceOn (_ptmiOid . tmInfo) oldMeta newMeta
    survivingTables =
      flip map (getOverlapWith (_ptmiOid . tmInfo) oldMeta newMeta) $ \(oldtm, newtm) ->
      (tmTable oldtm, getTableDiff oldtm newtm)

getSchemaChangeDeps
  :: forall b m. (QErrM m, CacheRM m, Backend b)
  => SourceName -> SchemaDiff b -> m [SourceObjId b]
getSchemaChangeDeps source schemaDiff = do
  -- Get schema cache
  sc <- askSchemaCache
  let tableIds =
        map
          (SOSourceObj source . AB.mkAnyBackend . SOITable @b)
          droppedTables
  -- Get the dependent of the dropped tables
  let tableDropDeps = concatMap (getDependentObjs sc) tableIds
  tableModDeps <- concat <$> traverse (uncurry (getTableChangeDeps source)) alteredTables
  -- return $ filter (not . isDirectDep) $
  return $ mapMaybe getIndirectDep $
    HS.toList $ HS.fromList $ tableDropDeps <> tableModDeps
  where
    SchemaDiff droppedTables alteredTables = schemaDiff

    getIndirectDep :: SchemaObjId -> Maybe (SourceObjId b)
    getIndirectDep (SOSourceObj s exists) =
      AB.unpackAnyBackend exists >>= \case
        srcObjId@(SOITableObj tn _) ->
          -- Indirect dependancy shouldn't be of same source and not among dropped tables
          if not (s == source && tn `HS.member` HS.fromList droppedTables)
            then Just srcObjId
            else Nothing
        srcObjId -> Just srcObjId
    getIndirectDep _ = Nothing

data FunctionDiff b
  = FunctionDiff
  { fdDropped :: ![FunctionName b]
  , fdAltered :: ![(FunctionName b, FunctionVolatility)]
  }
deriving instance (Backend b) => Show (FunctionDiff b)
deriving instance (Backend b) => Eq (FunctionDiff b)

getFuncDiff :: [FunctionMeta b] -> [FunctionMeta b] -> FunctionDiff b
getFuncDiff oldMeta newMeta =
  FunctionDiff droppedFuncs alteredFuncs
  where
    droppedFuncs = map fmFunction $ getDifferenceOn fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlapWith fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          alteredFunc = (fmFunction oldfm, fmType newfm)
      in bool Nothing (Just alteredFunc) isTypeAltered

getOverloadedFuncs
  :: (Backend b) => [FunctionName b] -> [FunctionMeta b] -> [FunctionName b]
getOverloadedFuncs trackedFuncs newFuncMeta =
  toList $ duplicates $ map fmFunction trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      fmFunction fm `elem` trackedFuncs

processSchemaDiff
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName
  -> TableCache b
  -> SchemaDiff b
  -> m ()
processSchemaDiff source preActionTables schemaDiff = do
  -- Purge the dropped tables
  dropTablesInMetadata @b source droppedTables

  for_ alteredTables $ \(oldQtn, tableDiff) -> do
    ti <- onNothing
      (M.lookup oldQtn preActionTables)
      (throw500 $ "old table metadata not found in cache : " <>> oldQtn)
    alterTableInMetadata source (_tiCoreInfo ti) tableDiff
  where
    SchemaDiff droppedTables alteredTables = schemaDiff

alterTableInMetadata
  :: forall m b
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName -> TableCoreInfo b -> TableDiff b -> m ()
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
  -- Drop custom column names for dropped columns
  alterCustomColumnNamesInMetadata source droppedCols ti
  maybe withOldTabName withNewTabName mNewName
  where
    TableDiff mNewName droppedCols alteredCols _ computedFieldDiff _ _ = tableDiff
    tableFields = _tciFieldInfoMap ti

    processComputedFieldDiff :: TableName b -> m ()
    processComputedFieldDiff table  = do
      let ComputedFieldDiff _ altered overloaded = computedFieldDiff
          getFunction = fmFunction . ccmFunctionMeta
      forM_ overloaded $ \(columnName, function) ->
        throw400 NotSupported $ "The function " <> function
        <<> " associated with computed field" <> columnName
        <<> " of table " <> table <<> " is being overloaded"
      forM_ altered $ \(old, new) ->
        if | (fmType . ccmFunctionMeta) new == FTVOLATILE ->
             throw400 NotSupported $ "The type of function " <> getFunction old
             <<> " associated with computed field " <> ccmName old
             <<> " of table " <> table <<> " is being altered to \"VOLATILE\""
           | otherwise -> pure ()

dropTablesInMetadata
  :: forall b m
   . ( MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName
  -> [TableName b]
  -> m ()
dropTablesInMetadata source droppedTables =
  forM_ droppedTables $
  \tn -> tell $ MetadataModifier $ metaSources.ix source.toSourceMetadata.(smTables @b) %~ OMap.delete tn

alterColumnsInMetadata
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName
  -> [(RawColumnInfo b, RawColumnInfo b)]
  -> FieldInfoMap (FieldInfo b)
  -> SchemaCache
  -> TableName b
  -> m ()
alterColumnsInMetadata source alteredCols fields sc tn = for_ alteredCols $
  \( RawColumnInfo oldName _ oldType _ _
   , RawColumnInfo newName _ newType _ _ ) -> do
    if | oldName /= newName ->
         renameColumnInMetadata oldName newName source tn fields

       | oldType /= newType -> do
          let colId =
                SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b tn
                  $ TOCol @b oldName
              typeDepObjs = getDependentObjsWith (== DROnType) sc colId

          unless (null typeDepObjs) $ throw400 DependencyError $
            "cannot change type of column " <> oldName <<> " in table "
            <> tn <<> " because of the following dependencies : " <>
            reportSchemaObjs typeDepObjs

       | otherwise -> pure ()

alterCustomColumnNamesInMetadata
  :: forall b m
   . (MonadWriter MetadataModifier m, BackendMetadata b)
  => SourceName
  -> [Column b]
  -> TableCoreInfo b
  -> m ()
alterCustomColumnNamesInMetadata source droppedCols ti = do
  let TableConfig customFields customColumnNames customName = _tciCustomConfig ti
      tn = _tciName ti
      modifiedCustomColumnNames = foldl' (flip M.delete) customColumnNames droppedCols
  when (modifiedCustomColumnNames /= customColumnNames) $
    tell $ MetadataModifier $
      tableMetadataSetter @b source tn.tmConfiguration .~
      TableConfig @b customFields modifiedCustomColumnNames customName
