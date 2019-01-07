module Hasura.RQL.DDL.Schema.Function where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Lift)

import qualified Hasura.GraphQL.Schema      as GS

import qualified Data.HashMap.Strict        as M
import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q


data PGTypType
  = PTBASE
  | PTCOMPOSITE
  | PTDOMAIN
  | PTENUM
  | PTRANGE
  | PTPSUEDO
  deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = drop 2} ''PGTypType)

data RawFuncInfo
  = RawFuncInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionType
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !T.Text
  , rfiReturnTypeType   :: !PGTypType
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![PGColType]
  , rfiInputArgNames    :: ![T.Text]
  } deriving (Show, Eq)
$(deriveFromJSON (aesonDrop 3 snakeCase) ''RawFuncInfo)

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

mkFunctionArgs :: [PGColType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs tys argNames =
  bool withNames withNoNames $ null argNames
  where
    withNoNames = flip map tys $ \ty -> FunctionArg Nothing ty
    withNames = zipWith mkArg argNames tys

    mkArg "" ty = FunctionArg Nothing ty
    mkArg n  ty = flip FunctionArg ty $ Just $ FunctionArgName n

mkFunctionInfo :: QualifiedFunction -> RawFuncInfo -> Q.TxE QErr FunctionInfo
mkFunctionInfo qf rawFuncInfo = do
  -- throw error if function has variadic arguments
  when hasVariadic $ throw400 NotSupported "function with \"VARIADIC\" parameters are not supported"
  -- throw error if return type is not composite type
  when (retTyTyp /= PTCOMPOSITE) $ throw400 NotSupported "function does not return a \"COMPOSITE\" type"
  -- throw error if function do not returns SETOF
  unless retSet $ throw400 NotSupported "function does not return a SETOF"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type \"VOLATILE\" is not supported now"

  let retTable = QualifiedTable retSn (TableName retN)

  -- throw error if return type is not a valid table
  assertTableExists retTable $ "return type " <> retTable <<> " is not a valid table"

  -- inpArgTyps <- mapM fetchTypNameFromOid inpArgTypIds
  let funcArgs = Seq.fromList $ mkFunctionArgs inpArgTyps inpArgNames
      dep = SchemaDependency (SOTable retTable) "table"
  return $ FunctionInfo qf False funTy funcArgs retTable [dep]
  where
    RawFuncInfo hasVariadic funTy retSn retN retTyTyp retSet inpArgTyps inpArgNames = rawFuncInfo

-- Build function info
getFunctionInfo :: QualifiedFunction -> Q.TxE QErr FunctionInfo
getFunctionInfo qf@(QualifiedFunction sn fn) = do
  -- fetch function details
  funcData <- Q.listQE defaultTxErrorHandler [Q.sql|
        SELECT
          row_to_json (
            (
              SELECT
                e
              FROM
                (
                  SELECT
                    has_variadic,
                    function_type,
                    return_type_schema,
                    return_type_name,
                    return_type_type,
                    returns_set,
                    input_arg_types,
                    input_arg_names
                ) AS e
            )
          ) AS "raw_function_info"
        FROM
          hdb_catalog.hdb_function_agg
        WHERE
          function_schema = $1 AND function_name = $2
     |] (sn, fn) False

  case funcData of
    []                              ->
      throw400 NotExists $ "no such function exists in postgres : " <>> qf
    [Identity (Q.AltJ rawFuncInfo)] -> mkFunctionInfo qf rawFuncInfo
    _                               ->
      throw400 NotSupported $
      "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

saveFunctionToCatalog :: QualifiedFunction -> Bool -> Q.TxE QErr ()
saveFunctionToCatalog (QualifiedFunction sn fn) isSystemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function" VALUES ($1, $2, $3)
                 |] (sn, fn, isSystemDefined) False

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
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
  -- check for conflicts in remote schema
  GS.checkConflictingNode defGCtx $ GS.qualFunctionToName qf

  trackFunctionP2Setup qf
  liftTx $ saveFunctionToCatalog qf False
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
