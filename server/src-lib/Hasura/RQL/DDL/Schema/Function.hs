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
import qualified Data.Sequence              as Seq
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
    "DOMAIN"    -> Just PTDOMAIN
    "ENUM"      -> Just PTENUM
    "RANGE"     -> Just PTRANGE
    "PSUEDO"    -> Just PTPSUEDO
    _           -> Nothing

assertTableExists :: QualifiedTable -> T.Text -> Q.TxE QErr ()
assertTableExists (QualifiedTable sn tn) err = do
  tableExists <- runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT exists(SELECT 1 from information_schema.tables
                      WHERE table_schema = $1
                      AND table_name = $2
                    )
          |] (sn, tn) False

  unless tableExists $ throw400 NotExists err

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
  -> Bool
  -> FunctionType
  -> T.Text
  -> T.Text
  -> PGTypType
  -> Bool
  -> [Int64]
  -> [T.Text]
  -> Q.TxE QErr FunctionInfo
mkFunctionInfo qf hasVariadic funTy retSn retN retTyTyp retSet inpArgTypIds inpArgNames = do
  -- throw error if function has variadic arguments
  when hasVariadic $ throw400 NotSupported "function with \"VARIADIC\" parameters are not supported"
  -- throw error if return type is not composite type
  when (retTyTyp /= PTCOMPOSITE) $ throw400 NotSupported "function does not return a \"COMPOSITE\" type"
  -- throw error if function do not returns SETOF
  unless retSet $ throw400 NotSupported "function does not return a SETOF"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type \"VOLATILE\" is not supported now"

  let retTable = QualifiedTable (SchemaName retSn) (TableName retN)

  -- throw error if return type is not a valid table
  assertTableExists retTable $ "return type " <> retTable <<> " is not a valid table"

  inpArgTyps <- mapM fetchTypNameFromOid inpArgTypIds
  let funcArgs = Seq.fromList $ mkFunctionArgs inpArgTyps inpArgNames
      dep = SchemaDependency (SOTable retTable) "table"
  return $ FunctionInfo qf False funTy funcArgs retTable [dep]

-- Build function info
getFunctionInfo :: QualifiedFunction -> Q.TxE QErr FunctionInfo
getFunctionInfo qf@(QualifiedFunction sn fn) = do
  -- fetch function details
  dets <- Q.listQE defaultTxErrorHandler [Q.sql|
              SELECT has_variadic, function_type, return_type_schema,
                     return_type_name, return_type_type, returns_set,
                     input_arg_types, input_arg_names
              FROM hdb_catalog.hdb_function_agg
             WHERE function_schema = $1 AND function_name = $2
          |] (sn, fn) False

  processDets dets
  where
    processDets [] =
      throw400 NotExists $ "no such function exists in postgres : " <>> qf
    processDets [( hasVar, fnTy, retTySn, retTyN, retTyTyp
                 , retSet, Q.AltJ argTys, Q.AltJ argNs
                 )] =
      mkFunctionInfo qf hasVar fnTy retTySn retTyN retTyTyp retSet argTys argNs
    processDets _ = throw400 NotSupported $
      "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

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

trackFunctionP1
  :: (CacheRM m, UserInfoM m, QErrM m) => TrackFunction -> m ()
trackFunctionP1 (TrackFunction qf) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf

trackFunctionP2Setup :: (QErrM m, CacheRWM m, MonadTx m)
                     => QualifiedFunction -> m ()
trackFunctionP2Setup qf = do
  fi <- withPathK "name" $ liftTx $ getFunctionInfo qf
  void $ askTabInfo $ fiReturnType fi
  addFunctionToCache fi

trackFunctionP2 :: (QErrM m, CacheRWM m, MonadTx m)
                => QualifiedFunction -> m RespBody
trackFunctionP2 qf = do
  trackFunctionP2Setup qf
  liftTx $ saveFunctionToCatalog qf
  return successMsg

runTrackFunc
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => TrackFunction -> m RespBody
runTrackFunc q = do
  trackFunctionP1 q
  trackFunctionP2 $ tfName q

newtype UnTrackFunction
  = UnTrackFunction
  { utfName :: QualifiedFunction }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

runUntrackFunc
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => UnTrackFunction -> m RespBody
runUntrackFunc (UnTrackFunction qf) = do
  adminOnly
  void $ askFunctionInfo qf
  liftTx $ delFunctionFromCatalog qf
  delFunctionFromCache qf
  return successMsg
