module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta(..)
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
import           Hasura.RQL.Types.Catalog
import           Hasura.Server.Utils      (duplicates)
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q

import           Control.Arrow            ((&&&))
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as HS
import qualified Data.List.NonEmpty       as NE

data FunctionMeta
  = FunctionMeta
  { fmOid      :: !OID
  , fmFunction :: !QualifiedFunction
  , fmType     :: !FunctionType
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
  { tmTable          :: !QualifiedTable
  , tmInfo           :: !CatalogTableInfo
  , tmComputedFields :: ![ComputedFieldMeta]
  } deriving (Show, Eq)

fetchTableMeta :: Q.Tx [TableMeta]
fetchTableMeta = Q.listQ $(Q.sqlFromFile "src-rsr/table_meta.sql") () False <&>
  map \(schema, name, Q.AltJ info, Q.AltJ computedFields) ->
    TableMeta (QualifiedObject schema name) info computedFields

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
  droppedFKeyConstraints computedFieldDiff uniqueOrPrimaryCons mNewDesc
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = _ctiColumns $ tmInfo oldtm
    newCols = _ctiColumns $ tmInfo newtm

    uniqueOrPrimaryCons = map _cName $
      maybeToList (_pkConstraint <$> _ctiPrimaryKey (tmInfo newtm))
        <> toList (_ctiUniqueConstraints $ tmInfo newtm)

    mNewDesc = _ctiDescription $ tmInfo newtm

    droppedCols = map prciName $ getDifference prciPosition oldCols newCols
    addedCols = getDifference prciPosition newCols oldCols
    existingCols = getOverlap prciPosition oldCols newCols
    alteredCols = filter (uncurry (/=)) existingCols

    -- foreign keys are considered dropped only if their oid
    -- and (ref-table, column mapping) are changed
    droppedFKeyConstraints = map (_cName . _fkConstraint) $ HS.toList $
      droppedFKeysWithOid `HS.intersection` droppedFKeysWithUniq
    tmForeignKeys = fmap unCatalogForeignKey . toList . _ctiForeignKeys . tmInfo
    droppedFKeysWithOid = HS.fromList $
      (getDifference (_cOid . _fkConstraint) `on` tmForeignKeys) oldtm newtm
    droppedFKeysWithUniq = HS.fromList $
      (getDifference mkFKeyUniqId `on` tmForeignKeys) oldtm newtm
    mkFKeyUniqId (ForeignKey _ reftn colMap) = (reftn, colMap)

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

    computedFieldDiff = ComputedFieldDiff droppedComputedFields alteredComputedFields
                      overloadedComputedFieldFunctions

getTableChangeDeps
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> TableDiff -> m [SchemaObjId]
getTableChangeDeps tn tableDiff = do
  sc <- askSchemaCache
  -- for all the dropped columns
  droppedColDeps <- fmap concat $ forM droppedCols $ \droppedCol -> do
    let objId = SOTableObj tn $ TOCol droppedCol
    return $ getDependentObjs sc objId
  -- for all dropped constraints
  droppedConsDeps <- fmap concat $ forM droppedFKeyConstraints $ \droppedCons -> do
    let objId = SOTableObj tn $ TOForeignKey droppedCons
    return $ getDependentObjs sc objId
  return $ droppedConsDeps <> droppedColDeps <> droppedComputedFieldDeps
  where
    TableDiff _ droppedCols _ _ droppedFKeyConstraints computedFieldDiff _ _ = tableDiff
    droppedComputedFieldDeps = map (SOTableObj tn . TOComputedField) $ _cfdDropped computedFieldDiff

data SchemaDiff
  = SchemaDiff
  { _sdDroppedTables :: ![QualifiedTable]
  , _sdAlteredTables :: ![(QualifiedTable, TableDiff)]
  } deriving (Show, Eq)

getSchemaDiff :: [TableMeta] -> [TableMeta] -> SchemaDiff
getSchemaDiff oldMeta newMeta =
  SchemaDiff droppedTables survivingTables
  where
    droppedTables = map tmTable $ getDifference (_ctiOid . tmInfo) oldMeta newMeta
    survivingTables =
      flip map (getOverlap (_ctiOid . tmInfo) oldMeta newMeta) $ \(oldtm, newtm) ->
      (tmTable oldtm, getTableDiff oldtm newtm)

getSchemaChangeDeps
  :: (QErrM m, CacheRM m)
  => SchemaDiff -> m [SchemaObjId]
getSchemaChangeDeps schemaDiff = do
  -- Get schema cache
  sc <- askSchemaCache
  let tableIds = map SOTable droppedTables
  -- Get the dependent of the dropped tables
  let tableDropDeps = concatMap (getDependentObjs sc) tableIds
  tableModDeps <- concat <$> traverse (uncurry getTableChangeDeps) alteredTables
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
        'type', f.function_type
      ) AS function_meta
    FROM
      hdb_catalog.hdb_function_agg f
    WHERE
      f.function_schema <> 'hdb_catalog'
    |] () False

data FunctionDiff
  = FunctionDiff
  { fdDropped :: ![QualifiedFunction]
  , fdAltered :: ![(QualifiedFunction, FunctionType)]
  } deriving (Show, Eq)

getFuncDiff :: [FunctionMeta] -> [FunctionMeta] -> FunctionDiff
getFuncDiff oldMeta newMeta =
  FunctionDiff droppedFuncs alteredFuncs
  where
    droppedFuncs = map fmFunction $ getDifference fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlap fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          alteredFunc = (fmFunction oldfm, fmType newfm)
      in bool Nothing (Just alteredFunc) $ isTypeAltered

getOverloadedFuncs
  :: [QualifiedFunction] -> [FunctionMeta] -> [QualifiedFunction]
getOverloadedFuncs trackedFuncs newFuncMeta =
  duplicates $ map fmFunction trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      fmFunction fm `elem` trackedFuncs
