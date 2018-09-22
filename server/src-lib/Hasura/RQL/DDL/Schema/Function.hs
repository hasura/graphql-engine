{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Int                   (Int64)
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified PostgreSQL.Binary.Decoding as PD


data PGTypType
  = PTBASE
  | PTCOMPOSITE
  | PTDOMAIN
  | PTENUM
  | PTRANGE
  | PTPSUEDO
  deriving (Show, Eq)

instance Q.FromCol PGTypType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "BASE"      -> Just PTBASE
    "COMPOSITE" -> Just PTCOMPOSITE
    "DOMAIN" -> Just PTDOMAIN
    "ENUM" -> Just PTENUM
    "RANGE" -> Just PTRANGE
    "PSUEDO" -> Just PTPSUEDO
    _ -> Nothing

assertTableExists :: QualifiedTable -> T.Text -> Q.TxE QErr ()
assertTableExists (QualifiedTable sn tn) err = do
  tableExists <- Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
            SELECT true from information_schema.tables
             WHERE table_schema = $1
               AND table_name = $2;
                           |] (sn, tn) False

  -- if no columns are found, there exists no such view/table
  unless (tableExists == [Identity True]) $
    throw400 NotExists err

fetchTypNameFromOid :: Int64 -> Q.TxE QErr PGColType
fetchTypNameFromOid tyId =
  Q.getAltJ . runIdentity . Q.getRow <$>
    Q.withQE defaultTxErrorHandler [Q.sql|
          SELECT to_json(t.typname)
            FROM pg_catalog.pg_type t
           WHERE t.oid = $1
        |] (Identity tyId) False

mkFunctionArgs :: [PGColType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs tys argNames =
  bool withNames withNoNames $ null argNames
  where
    withNoNames = flip map tys $ \ty -> FunctionArg Nothing ty
    withNames = zipWith mkArg argNames tys

    mkArg "" ty = FunctionArg Nothing ty
    mkArg n  ty = flip FunctionArg ty $ Just $ FunctionArgName n

mkFunctionInfo
  :: QualifiedFunction
  -> FunctionType
  -> T.Text
  -> T.Text
  -> PGTypType
  -> Bool
  -> [Int64]
  -> [T.Text]
  -> Q.TxE QErr FunctionInfo
mkFunctionInfo qf funTy retSn retN retTyTyp retSet inpArgTypIds inpArgNames = do
  -- throw error if return type is not composite type
  when (retTyTyp /= PTCOMPOSITE) $ throw400 NotSupported "function does not return a COMPOSITE type"
  -- throw error if function do not returns SETOF
  unless retSet $ throw400 NotSupported "function does not return a SETOF"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type VOLATILE is not supported now"

  let retTable = QualifiedTable (SchemaName retSn) (TableName retN)

  -- throw error if return type is not a table
  assertTableExists retTable $ "return type table " <> retTable <<> " not found in postgres"

  inpArgTyps <- mapM fetchTypNameFromOid inpArgTypIds
  let funcArgs = mkFunctionArgs inpArgTyps inpArgNames
      dep = SchemaDependency (SOTable retTable) "table"
  return $ FunctionInfo qf False funTy funcArgs retTable [dep]

-- Build function info
getFunctionInfo :: QualifiedFunction -> Q.TxE QErr FunctionInfo
getFunctionInfo qf@(QualifiedFunction sn fn) = do
  functionExists <- Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
            SELECT true from information_schema.routines
             WHERE routine_schema = $1
               AND routine_name = $2
                             |] (sn, fn) False

  -- if no columns are found, there exists no such function
  unless (functionExists == [Identity True]) $
    throw400 NotExists $ "no such function exists in postgres : " <>> qf

  -- fetch function details
  dets <- Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
              SELECT
              (
                CASE
                  WHEN p.provolatile = 'i'::char THEN 'IMMUTABLE'::text
                  WHEN p.provolatile = 's'::char THEN 'STABLE'::text
                  WHEN p.provolatile = 'v'::char THEN 'VOLATILE'::text
                  else NULL::text
                END
              ) as function_type,
              r.type_udt_schema as return_type_schema,
              r.type_udt_name as return_type_name,
              (
                CASE
                  WHEN t.typtype = 'b'::char THEN 'BASE'::text
                  WHEN t.typtype = 'c'::char THEN 'COMPOSITE'::text
                  WHEN t.typtype = 'd':: char THEN 'DOMAIN'::text
                  WHEN t.typtype = 'e'::char THEN 'ENUM'::text
                  WHEN t.typtype = 'r'::char THEN 'RANGE'::text
                  WHEN t.typtype = 'p'::char THEN 'PSUEDO'::text
                  else NULL::text
                END
              ) as return_type_type,
              p.proretset as returns_set,
	      to_json(coalesce(p.proallargtypes, p.proargtypes)::int[]) as input_arg_types,
	      to_json(coalesce(p.proargnames, array[]::text[])) as input_arg_names

              FROM information_schema.routines r
                   JOIN pg_catalog.pg_proc p on (p.proname = r.routine_name)
                   JOIN pg_catalog.pg_type t on (t.oid = p.prorettype)
              WHERE r.routine_schema = $1
                AND r.routine_name = $2
          |] (sn, fn) False

  processDets dets
  where
    processDets (fnTy, retTySn, retTyN, retTyTyp, retSet, Q.AltJ argTys, Q.AltJ argNs) =
      mkFunctionInfo qf fnTy retTySn retTyN retTyTyp retSet argTys argNs

saveFunctionToCatalog :: QualifiedFunction -> Q.TxE QErr ()
saveFunctionToCatalog (QualifiedFunction sn fn) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function" VALUES ($1, $2)
                 |] (sn, fn) False

delFunctionFromCatalog :: QualifiedFunction -> Q.TxE QErr ()
delFunctionFromCatalog (QualifiedFunction sn fn) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         DELETE FROM hdb_catalog.hdb_function
         WHERE function_schema = $1
           AND function_name = $2
         |] (sn, fn) False

newtype TrackFunction
  = TrackFunction
  { tfName :: QualifiedFunction}
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

trackFunctionP1 :: TrackFunction -> P1 ()
trackFunctionP1 (TrackFunction qf) = do
  adminOnly
  rawSchemaCache <- getSchemaCache <$> lift ask
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf

trackFunctionP2Setup :: (P2C m) => QualifiedFunction -> m ()
trackFunctionP2Setup qf = do
  fi <- liftTx $ getFunctionInfo qf
  void $ getTableInfoFromCache $ fiReturnType fi
  addFunctionToCache fi

trackFunctionP2 :: (P2C m) => QualifiedFunction -> m RespBody
trackFunctionP2 qf = do
  trackFunctionP2Setup qf
  liftTx $ saveFunctionToCatalog qf
  return successMsg

instance HDBQuery TrackFunction where

  type Phase1Res TrackFunction = ()
  phaseOne = trackFunctionP1

  phaseTwo (TrackFunction qf) _ = trackFunctionP2 qf

  schemaCachePolicy = SCPReload

newtype UnTrackFunction
  = UnTrackFunction
  { utfName :: QualifiedFunction }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

instance HDBQuery UnTrackFunction where

  type Phase1Res UnTrackFunction = ()
  phaseOne (UnTrackFunction qf) = do
    adminOnly
    void $ askFunctionInfo qf

  phaseTwo (UnTrackFunction qf) _ = do
    liftTx $ delFunctionFromCatalog qf
    delFunctionFromCache qf
    return successMsg

  schemaCachePolicy = SCPReload
