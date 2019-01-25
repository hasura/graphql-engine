module Hasura.RQL.DDL.Schema.Diff
  ( TableMeta(..)
  , PGColMeta(..)
  , ConstraintMeta(..)
  , fetchTableMeta

  , TableDiff(..)
  , getTableDiff
  , getTableChangeDeps

  , SchemaDiff(..)
  , getSchemaDiff
  , getSchemaChangeDeps

  , FunctionMeta(..)
  , fetchFunctionMeta
  , getDroppedFuncs
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query   as Q

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
  } deriving (Show, Eq)

fetchTableMeta :: Q.Tx [TableMeta]
fetchTableMeta = do
  res <- Q.listQ [Q.sql|
    SELECT
        t.table_schema,
        t.table_name,
        t.table_oid,
        coalesce(c.columns, '[]') as columns,
        coalesce(f.constraints, '[]') as constraints
    FROM
        (SELECT
             c.oid as table_oid,
             c.relname as table_name,
             n.nspname as table_schema
         FROM
             pg_catalog.pg_class c
         JOIN
             pg_catalog.pg_namespace as n
           ON
             c.relnamespace = n.oid
        ) t
        LEFT OUTER JOIN
        (SELECT
             table_schema,
             table_name,
             json_agg((SELECT r FROM (SELECT column_name, udt_name AS data_type, ordinal_position, is_nullable::boolean) r)) as columns
         FROM
             information_schema.columns
         GROUP BY
             table_schema, table_name) c
        ON (t.table_schema = c.table_schema AND t.table_name = c.table_name)
        LEFT OUTER JOIN
        (SELECT
             tc.table_schema,
             tc.table_name,
             json_agg(
              json_build_object(
                  'name', tc.constraint_name,
                  'oid', r.oid::integer,
                  'type', tc.constraint_type
                  )
             ) as constraints
         FROM
             information_schema.table_constraints tc
             JOIN pg_catalog.pg_constraint r
             ON tc.constraint_name = r.conname
         GROUP BY
             table_schema, table_name) f
        ON (t.table_schema = f.table_schema AND t.table_name = f.table_name)
    WHERE
        t.table_schema NOT LIKE 'pg_%'
        AND t.table_schema <> 'information_schema'
        AND t.table_schema <> 'hdb_catalog'
                |] () False
  forM res $ \(ts, tn, toid, cols, constrnts) ->
    return $ TableMeta toid (QualifiedObject ts tn) (Q.getAltJ cols) (Q.getAltJ constrnts)

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
      [cmName cm | cm <- tmConstraints newtm, isUniqueOrPrimary $ cmType cm]

    droppedCols =
      map pcmColumnName $ getDifference pcmOrdinalPosition oldCols newCols

    addedCols =
      map pcmToPci $ getDifference pcmOrdinalPosition newCols oldCols

    existingCols = getOverlap pcmOrdinalPosition oldCols newCols

    pcmToPci (PGColMeta colName _ colType isNullable)
      = PGColInfo colName colType isNullable

    alteredCols =
      flip map (filter (uncurry (/=)) existingCols) $ \(pcmo, pcmn) ->
      (pcmToPci pcmo, pcmToPci pcmn)

    droppedFKeyConstraints = map cmName $
      filter (isForeignKey . cmType) $ getDifference cmOid
      (tmConstraints oldtm) (tmConstraints newtm)

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

    isDirectDep (SOTableObj tn _) = tn `HS.member` (HS.fromList droppedTables)
    isDirectDep _                 = False

data FunctionMeta
  = FunctionMeta
  { fmOid      :: !Int
  , fmFunction :: !QualifiedFunction
  } deriving (Show, Eq)

fetchFunctionMeta :: Q.Tx [FunctionMeta]
fetchFunctionMeta = do
  res <- Q.listQ [Q.sql|
    SELECT
        f.function_schema,
        f.function_name,
        p.oid
    FROM hdb_catalog.hdb_function_agg f
         JOIN pg_catalog.pg_proc p ON (p.proname = f.function_name)
    WHERE
        f.function_schema <> 'hdb_catalog'
                  |] () False
  forM res $ \(sn, fn, foid) ->
    return $ FunctionMeta foid $ QualifiedObject sn fn

getDroppedFuncs :: [FunctionMeta] -> [FunctionMeta] -> [QualifiedFunction]
getDroppedFuncs oldMeta newMeta =
  map fmFunction $ getDifference fmOid oldMeta newMeta
