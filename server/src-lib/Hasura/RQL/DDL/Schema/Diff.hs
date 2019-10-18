module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta(..)
  , PGColMeta(..)
  , ConstraintMeta(..)
  , fetchTableMeta
  , ComputedFieldMeta(..)

  , getDifference

  , TableDiff(..)
  , getTableDiff
  , getTableChangeDeps
  , ComputedFieldDiff(..)

  , SchemaDiff(..)
  , getSchemaDiff
  , getSchemaChangeDeps

  , FunctionMeta(..)
  , fetchFunctionMeta
  , FunctionDiff(..)
  , getFuncDiff
  , getOverloadedFuncs
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils (duplicates)
import           Hasura.SQL.Types

import qualified Database.PG.Query   as Q

import           Control.Arrow       ((&&&))
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as HS
import qualified Data.List.NonEmpty  as NE

data PGColMeta
  = PGColMeta
  { pcmColumnName      :: !PGCol
  , pcmOrdinalPosition :: !Int
  , pcmDataType        :: !PGScalarType
  , pcmIsNullable      :: !Bool
  , pcmReferences      :: ![QualifiedTable]
  , pcmDescription     :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''PGColMeta)

data ConstraintMeta
  = ConstraintMeta
  { cmName :: !ConstraintName
  , cmOid  :: !Int
  , cmType :: !ConstraintType
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''ConstraintMeta)

data FunctionMeta
  = FunctionMeta
  { fmOid         :: !Int
  , fmFunction    :: !QualifiedFunction
  , fmType        :: !FunctionType
  , fmDescription :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 snakeCase) ''FunctionMeta)

data ComputedFieldMeta
  = ComputedFieldMeta
  { ccmName         :: !ComputedFieldName
  , ccmFunctionMeta :: !FunctionMeta
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ComputedFieldMeta)

data TableMeta
  = TableMeta
  { tmOid            :: !Int
  , tmTable          :: !QualifiedTable
  , tmDescription    :: !(Maybe PGDescription)
  , tmColumns        :: ![PGColMeta]
  , tmConstraints    :: ![ConstraintMeta]
  , tmForeignKeys    :: ![ForeignKey]
  , tmComputedFields :: ![ComputedFieldMeta]
  } deriving (Show, Eq)

fetchTableMeta :: Q.Tx [TableMeta]
fetchTableMeta = do
  res <- Q.listQ $(Q.sqlFromFile "src-rsr/table_meta.sql") () False
  forM res $ \(ts, tn, toid, descM, cols, constrnts, fkeys, computedCols) ->
    return $ TableMeta toid (QualifiedObject ts tn) descM (Q.getAltJ cols)
             (Q.getAltJ constrnts) (Q.getAltJ fkeys) (Q.getAltJ computedCols)

getOverlap :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [(v, v)]
getOverlap getKey left right =
  M.elems $ M.intersectionWith (,) (mkMap left) (mkMap right)
  where
    mkMap = M.fromList . map (\v -> (getKey v, v))

getDifference :: (Eq k, Hashable k) => (v -> k) -> [v] -> [v] -> [v]
getDifference getKey left right =
  M.elems $ M.difference (mkMap left) (mkMap right)
  where
    mkMap = M.fromList . map (\v -> (getKey v, v))

data ComputedFieldDiff
  = ComputedFieldDiff
  { _cfdDropped    :: [ComputedFieldName]
  , _cfdAltered    :: [(ComputedFieldMeta, ComputedFieldMeta)]
  , _cfdOverloaded :: [(ComputedFieldName, QualifiedFunction)]
  } deriving (Show, Eq)

data TableDiff
  = TableDiff
  { _tdNewName         :: !(Maybe QualifiedTable)
  , _tdDroppedCols     :: ![PGCol]
  , _tdAddedCols       :: ![PGRawColumnInfo]
  , _tdAlteredCols     :: ![(PGRawColumnInfo, PGRawColumnInfo)]
  , _tdDroppedFKeyCons :: ![ConstraintName]
  , _tdComputedFields  :: !ComputedFieldDiff
  -- The final list of uniq/primary constraint names
  -- used for generating types on_conflict clauses
  -- TODO: this ideally should't be part of TableDiff
  , _tdUniqOrPriCons   :: ![ConstraintName]
  , _tdNewDescription  :: !(Maybe PGDescription)
  } deriving (Show, Eq)

getTableDiff :: TableMeta -> TableMeta -> TableDiff
getTableDiff oldtm newtm =
  TableDiff mNewName droppedCols addedCols alteredCols
  droppedFKeyConstraints computedColDiff uniqueOrPrimaryCons mNewDesc
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = tmColumns oldtm
    newCols = tmColumns newtm

    uniqueOrPrimaryCons =
      [cmName cm | cm <- tmConstraints newtm, isUniqueOrPrimary (cmType cm)]

    mNewDesc = tmDescription newtm

    droppedCols =
      map pcmColumnName $ getDifference pcmOrdinalPosition oldCols newCols

    addedCols =
      map pcmToPci $ getDifference pcmOrdinalPosition newCols oldCols

    existingCols = getOverlap pcmOrdinalPosition oldCols newCols

    pcmToPci (PGColMeta colName _ colType isNullable references descM)
      = PGRawColumnInfo colName colType isNullable references descM

    alteredCols =
      flip map (filter (uncurry (/=)) existingCols) $ pcmToPci *** pcmToPci

    -- foreign keys are considered dropped only if their oid
    -- and (ref-table, column mapping) are changed
    droppedFKeyConstraints = map _fkConstraint $ HS.toList $
      droppedFKeysWithOid `HS.intersection` droppedFKeysWithUniq

    droppedFKeysWithOid = HS.fromList $
      getDifference _fkOid (tmForeignKeys oldtm) (tmForeignKeys newtm)

    droppedFKeysWithUniq = HS.fromList $
      getDifference mkFKeyUniqId (tmForeignKeys oldtm) (tmForeignKeys newtm)

    mkFKeyUniqId (ForeignKey _ reftn _ _ colMap) = (reftn, colMap)

    -- calculate computed field diff
    oldComputedFieldMeta = tmComputedFields oldtm
    newComputedFieldMeta = tmComputedFields newtm

    droppedComputedFields = map ccmName $
      getDifference (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    alteredComputedFields =
      getOverlap (fmOid . ccmFunctionMeta) oldComputedFieldMeta newComputedFieldMeta

    overloadedComputedFieldFunctions =
      let getFunction = fmFunction . ccmFunctionMeta
          getSecondElement (_ NE.:| list) = listToMaybe list
      in mapMaybe (fmap ((&&&) ccmName getFunction) . getSecondElement) $
         flip NE.groupBy newComputedFieldMeta $ \l r ->
         ccmName l == ccmName r && getFunction l == getFunction r

    computedColDiff = ComputedFieldDiff droppedComputedFields alteredComputedFields
                      overloadedComputedFieldFunctions

getTableChangeDeps
  :: (QErrM m, CacheRWM m)
  => TableInfo PGColumnInfo -> TableDiff -> m [SchemaObjId]
getTableChangeDeps ti tableDiff = do
  sc <- askSchemaCache
  -- for all the dropped columns
  droppedColDeps <- fmap concat $ forM droppedCols $ \droppedCol -> do
    let objId = SOTableObj tn $ TOCol droppedCol
    return $ getDependentObjs sc objId
  -- for all dropped constraints
  droppedConsDeps <- fmap concat $ forM droppedFKeyConstraints $ \droppedCons -> do
    let objId = SOTableObj tn $ TOCons droppedCons
    return $ getDependentObjs sc objId
  return $ droppedConsDeps <> droppedColDeps <> droppedComputedFieldDeps
  where
    tn = _tiName ti
    TableDiff _ droppedCols _ _ droppedFKeyConstraints computedColDiff _ _ = tableDiff
    droppedComputedFieldDeps = map (SOTableObj tn . TOComputedField) $ _cfdDropped computedColDiff

data SchemaDiff
  = SchemaDiff
  { _sdDroppedTables :: ![QualifiedTable]
  , _sdAlteredTables :: ![(QualifiedTable, TableDiff)]
  } deriving (Show, Eq)

getSchemaDiff :: [TableMeta] -> [TableMeta] -> SchemaDiff
getSchemaDiff oldMeta newMeta =
  SchemaDiff droppedTables survivingTables
  where
    droppedTables   = map tmTable $ getDifference tmOid oldMeta newMeta
    survivingTables =
      flip map (getOverlap tmOid oldMeta newMeta) $ \(oldtm, newtm) ->
      (tmTable oldtm, getTableDiff oldtm newtm)

getSchemaChangeDeps
  :: (QErrM m, CacheRWM m)
  => SchemaDiff -> m [SchemaObjId]
getSchemaChangeDeps schemaDiff = do
  -- Get schema cache
  sc <- askSchemaCache
  let tableIds = map SOTable droppedTables
  -- Get the dependent of the dropped tables
  let tableDropDeps = concatMap (getDependentObjs sc) tableIds
  tableModDeps <- fmap concat $ forM alteredTables $ \(oldQtn, tableDiff) -> do
    ti <- case M.lookup oldQtn $ scTables sc of
      Just ti -> return ti
      Nothing -> throw500 $ "old table metadata not found in cache : " <>> oldQtn
    getTableChangeDeps ti tableDiff
  return $ filter (not . isDirectDep) $
    HS.toList $ HS.fromList $ tableDropDeps <> tableModDeps
  where
    SchemaDiff droppedTables alteredTables = schemaDiff

    isDirectDep (SOTableObj tn _) = tn `HS.member` HS.fromList droppedTables
    isDirectDep _                 = False

fetchFunctionMeta :: Q.Tx [FunctionMeta]
fetchFunctionMeta =
  map (Q.getAltJ . runIdentity) <$> Q.listQ [Q.sql|
    SELECT
      json_build_object(
        'oid', f.function_oid,
        'function', json_build_object('name', f.function_name, 'schema', f.function_schema),
        'type', f.function_type,
        'description', f.description
      ) AS function_meta
    FROM
      hdb_catalog.hdb_function_agg f
    WHERE
      f.function_schema <> 'hdb_catalog'
    |] () False

data FunctionDiff
  = FunctionDiff
  { fdDropped :: ![QualifiedFunction]
  , fdAltered :: ![(QualifiedFunction, FunctionType, Maybe PGDescription)]
  } deriving (Show, Eq)

getFuncDiff :: [FunctionMeta] -> [FunctionMeta] -> FunctionDiff
getFuncDiff oldMeta newMeta =
  FunctionDiff droppedFuncs alteredFuncs
  where
    droppedFuncs = map fmFunction $ getDifference fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlap fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          isDescriptionAltered = fmDescription oldfm /= fmDescription newfm
          alteredFunc = (fmFunction oldfm, fmType newfm, fmDescription newfm)
      in bool Nothing (Just alteredFunc) $ isTypeAltered || isDescriptionAltered

getOverloadedFuncs
  :: [QualifiedFunction] -> [FunctionMeta] -> [QualifiedFunction]
getOverloadedFuncs trackedFuncs newFuncMeta =
  duplicates $ map fmFunction trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      fmFunction fm `elem` trackedFuncs
