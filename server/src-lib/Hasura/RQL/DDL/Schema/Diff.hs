module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta(..)
  , PGColMeta(..)
  , ConstraintMeta(..)
  , fetchTableMeta

  , getDifference

  , TableDiff(..)
  , getTableDiff
  , getTableChangeDeps

  , SchemaDiff(..)
  , getSchemaDiff
  , getSchemaChangeDeps

  , FunctionMeta(..)
  , funcFromMeta
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

import           Control.Arrow       ((***))
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as HS

data PGColMeta
  = PGColMeta
  { pcmColumnName      :: !PGCol
  , pcmOrdinalPosition :: !Int
  , pcmDataType        :: !PGColType
  , pcmIsNullable      :: !Bool
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''PGColMeta)

data ConstraintMeta
  = ConstraintMeta
  { cmName :: !ConstraintName
  , cmOid  :: !Int
  , cmType :: !ConstraintType
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''ConstraintMeta)

data TableMeta
  = TableMeta
  { tmOid         :: !Int
  , tmTable       :: !QualifiedTable
  , tmColumns     :: ![PGColMeta]
  , tmConstraints :: ![ConstraintMeta]
  , tmForeignKeys :: ![ForeignKey]
  } deriving (Show, Eq)

fetchTableMeta :: Q.Tx [TableMeta]
fetchTableMeta = do
  res <- Q.listQ $(Q.sqlFromFile "src-rsr/table_meta.sql") () False
  forM res $ \(ts, tn, toid, cols, constrnts, fkeys) ->
    return $ TableMeta toid (QualifiedObject ts tn) (Q.getAltJ cols)
             (Q.getAltJ constrnts) (Q.getAltJ fkeys)

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

data TableDiff
  = TableDiff
  { _tdNewName         :: !(Maybe QualifiedTable)
  , _tdDroppedCols     :: ![PGCol]
  , _tdAddedCols       :: ![PGColInfo]
  , _tdAlteredCols     :: ![(PGColInfo, PGColInfo)]
  , _tdDroppedFKeyCons :: ![ConstraintName]
  -- The final list of uniq/primary constraint names
  -- used for generating types on_conflict clauses
  -- TODO: this ideally should't be part of TableDiff
  , _tdUniqOrPriCons   :: ![ConstraintName]
  } deriving (Show, Eq)

getTableDiff :: TableMeta -> TableMeta -> TableDiff
getTableDiff oldtm newtm =
  TableDiff mNewName droppedCols addedCols alteredCols
  droppedFKeyConstraints uniqueOrPrimaryCons
  where
    mNewName = bool (Just $ tmTable newtm) Nothing $ tmTable oldtm == tmTable newtm
    oldCols = tmColumns oldtm
    newCols = tmColumns newtm

    uniqueOrPrimaryCons =
      [cmName cm | cm <- tmConstraints newtm, isUniqueOrPrimary (cmType cm)]

    droppedCols =
      map pcmColumnName $ getDifference pcmOrdinalPosition oldCols newCols

    addedCols =
      map pcmToPci $ getDifference pcmOrdinalPosition newCols oldCols

    existingCols = getOverlap pcmOrdinalPosition oldCols newCols

    pcmToPci (PGColMeta colName _ colType isNullable)
      = PGColInfo colName colType isNullable

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

getTableChangeDeps
  :: (QErrM m, CacheRWM m)
  => TableInfo -> TableDiff -> m [SchemaObjId]
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
  return $ droppedConsDeps <> droppedColDeps
  where
    tn = tiName ti
    TableDiff _ droppedCols _ _ droppedFKeyConstraints _ = tableDiff

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

data FunctionMeta
  = FunctionMeta
  { fmOid    :: !Int
  , fmSchema :: !SchemaName
  , fmName   :: !FunctionName
  , fmType   :: !FunctionType
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 snakeCase) ''FunctionMeta)

funcFromMeta :: FunctionMeta -> QualifiedFunction
funcFromMeta fm = QualifiedObject (fmSchema fm) (fmName fm)

fetchFunctionMeta :: Q.Tx [FunctionMeta]
fetchFunctionMeta =
  map (Q.getAltJ . runIdentity) <$> Q.listQ [Q.sql|
    SELECT
      json_build_object(
        'oid', p.oid :: integer,
        'schema', f.function_schema,
        'name', f.function_name,
        'type', f.function_type
      ) AS function_meta
    FROM
      hdb_catalog.hdb_function_agg f
      JOIN pg_catalog.pg_proc p ON (p.proname = f.function_name)
      JOIN pg_catalog.pg_namespace pn ON (
        pn.oid = p.pronamespace
        AND pn.nspname = f.function_schema
      )
    WHERE
      f.function_schema <> 'hdb_catalog'
    GROUP BY p.oid, f.function_schema, f.function_name, f.function_type
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
    droppedFuncs = map funcFromMeta $ getDifference fmOid oldMeta newMeta
    alteredFuncs = mapMaybe mkAltered $ getOverlap fmOid oldMeta newMeta
    mkAltered (oldfm, newfm) =
      let isTypeAltered = fmType oldfm /= fmType newfm
          alteredFunc = (funcFromMeta oldfm, fmType newfm)
      in bool Nothing (Just alteredFunc) isTypeAltered

getOverloadedFuncs
  :: [QualifiedFunction] -> [FunctionMeta] -> [QualifiedFunction]
getOverloadedFuncs trackedFuncs newFuncMeta =
  duplicates $ map funcFromMeta trackedMeta
  where
    trackedMeta = flip filter newFuncMeta $ \fm ->
      funcFromMeta fm `elem` trackedFuncs
