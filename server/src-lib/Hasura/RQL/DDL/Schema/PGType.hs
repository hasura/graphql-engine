module Hasura.RQL.DDL.Schema.PGType where

import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Database.PG.Query as Q

getPGTyInfoMap :: Q.TxE QErr PGTyInfoMaps
getPGTyInfoMap = do
  pgTyInfo <- Q.catchE defaultTxErrorHandler $
    Q.listQ $(Q.sqlFromFile "src-rsr/pg_type_info.sql") () True
  return $ mkPGTyMaps $ map (Q.getAltJ . runIdentity) pgTyInfo
