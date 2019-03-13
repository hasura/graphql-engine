module Hasura.RQL.DDL.Schema.PGType where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types


import qualified Data.HashSet         as Set
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as T
import qualified Database.PG.Query    as Q


getPGTyInfoMap :: Q.TxE QErr PGTyInfoMaps
getPGTyInfoMap = do
  pgTyInfo <- Q.catchE defaultTxErrorHandler $
    Q.listQ $(Q.sqlFromFile "src-rsr/pg_type_info.sql") () True
  return $ mkPGTyMaps $ map (Q.getAltJ . runIdentity) pgTyInfo

toPGColTysWithCaching :: (QErrM m, CacheRWM m, MonadTx m) => [PGColOidInfo] -> m [PGColType]
toPGColTysWithCaching oids = do
  curTysCache <- addPGTysToCache $ Set.fromList oids
  forM oids $ \oid -> onNothing (Map.lookup oid curTysCache) $ throw500 $
    "Could not find Postgres type with oid info" <> T.pack (show oid)

getPGColTysMap :: Set.HashSet PGColOidInfo -> Q.TxE QErr (Map.HashMap PGColOidInfo PGColType)
getPGColTysMap ctis = do
  tim <- getPGTyInfoMap
  fmap Map.fromList $ forM (Set.toList ctis) $ \x -> fmap ((,) x) $ onNothing (getPGColTy tim x) $ errMsg x
  where
    errMsg x = throw500 $ "Could not find Postgres type for oid " <> T.pack (show $ pcoiOid x)

-- Do a union of given types and the required types from pg_catalog
addPGTysToCache :: (QErrM m, CacheRWM m, MonadTx m) => Set.HashSet PGColOidInfo -> m (Map.HashMap PGColOidInfo PGColType)
addPGTysToCache i = do
  tysCache <- fmap scTyMap askSchemaCache
  let inCache x =  isJust $ Map.lookup x tysCache
  if (all inCache i)
    then return tysCache
    else updatePGTysCache i

updatePGTysCache :: (CacheRWM m, MonadTx m) => Set.HashSet PGColOidInfo -> m (Map.HashMap PGColOidInfo PGColType)
updatePGTysCache iTys = do
  cTys <- liftTx $ getCatalogTys
  updTysMap <- liftTx $ getPGColTysMap $ Set.union cTys iTys
  modPGTyCache updTysMap
  return updTysMap

getCatalogTys :: Q.TxE QErr (Set.HashSet PGColOidInfo)
getCatalogTys = do
  res <- Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
      SELECT
        ft.argOid :: int as oid,
        case
          when elemTy.oid is not null
          then 1
          else 0
        end as dims
      FROM
        (
          SELECT DISTINCT unnest(COALESCE(pp.proallargtypes, pp.proargtypes::oid[])) as argOid
          FROM
            hdb_catalog.hdb_function hp
            left outer join pg_proc pp
            on
            ( hp.function_name = pp.proname and
              hp.function_schema = pp.pronamespace::regnamespace::text
            )
        ) ft
        left outer join pg_type elemTy
        on ft.argOid = elemTy.typarray

      UNION

      SELECT DISTINCT
        td.atttypid :: int as oid,
        td.attndims as dims
      FROM
        hdb_catalog.hdb_table ht
        left outer join information_schema.columns c
        on ht.table_schema = c.table_schema and ht.table_name = c.table_name
        left outer join (
           select pc.relnamespace,
                  pc.relname,
                  pa.attname,
                  pa.attndims,
                  pa.atttypid
           from pg_attribute pa
           left join pg_class pc
           on pa.attrelid = pc.oid
        ) td on
        ( c.table_schema::regnamespace::oid = td.relnamespace
          AND c.table_name = td.relname
          AND c.column_name = td.attname
        )
    |] () False
  return $ Set.fromList $ flip map res $ \(oid, dims) -> PGColOidInfo oid (fromIntegral (dims :: Int))
