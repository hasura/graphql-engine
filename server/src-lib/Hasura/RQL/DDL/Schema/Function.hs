{-# LANGUAGE QuasiQuotes #-}
module Hasura.RQL.DDL.Schema.Function where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils          (isValidName, showNames)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Data.HashMap.Strict           as M
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q

data RawFuncInfo
  = RawFuncInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionType
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !PGColType
  , rfiReturnTypeType   :: !PGTypType
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![QualifiedPGType]
  , rfiInputArgNames    :: ![T.Text]
  , rfiReturnsTable     :: !Bool
  } deriving (Show, Eq)
$(deriveFromJSON (aesonDrop 3 snakeCase) ''RawFuncInfo)

mkFunctionArgs :: [QualifiedPGType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs tys argNames =
  bool withNames withNoNames $ null argNames
  where
    withNoNames = flip map tys $ \ty -> FunctionArg Nothing $ _qptName ty
    withNames = zipWith mkArg argNames $ map _qptName tys

    mkArg "" ty = FunctionArg Nothing ty
    mkArg n  ty = flip FunctionArg ty $ Just $ FunctionArgName n

validateFuncArgs :: MonadError QErr m => [FunctionArg] -> m ()
validateFuncArgs args =
  unless (null invalidArgs) $ throw400 NotSupported $
    "arguments: " <> showNames invalidArgs
    <> " are not in compliance with GraphQL spec"
  where
    funcArgsText = mapMaybe (fmap getFuncArgNameTxt . faName) args
    invalidArgs = filter (not . isValidName) $ map G.Name funcArgsText

mkFunctionInfo
  :: (MonadError QErr m)
  => QualifiedFunction -> RawFuncInfo -> m SQLFunction
mkFunctionInfo qf rawFuncInfo = do
  -- throw error if function has variadic arguments
  when hasVariadic $ throw400 NotSupported "function with \"VARIADIC\" parameters are not supported"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type \"VOLATILE\" is not supported now"

  maybe asQueryFunc asCompColFunc firstArgAsTable
  where
    RawFuncInfo hasVariadic funTy retSn retN retTyTyp
                retSet inpArgTyps inpArgNames returnsTab
                = rawFuncInfo
    returnType = QualifiedPGType retSn retN retTyTyp

    firstArgAsTable = case inpArgTyps of
      []   -> Nothing
      x:xs -> (, xs) <$> asTableType x

    getFuncArgs types names = do
      let args = mkFunctionArgs types names
      validateFuncArgs args
      return $ Seq.fromList args

    qFuncPrefixMsg =
      "function " <> qf <<> " being tracked as Query Function; "

    asQueryFunc = modifyErr (qFuncPrefixMsg <>) $ do
      -- throw error if function do not returns SETOF
      unless retSet $
        throw400 NotSupported "function does not return a SETOF"
      -- throw error if return type is not a valid table
      unless returnsTab $
        throw400 NotSupported "function does not return a SETOF table"
      -- throw error if any of input arguments is table type
      when (any (isJust . asTableType) inpArgTyps) $
        throw400 NotSupported "function input argument has atleast one composite type"
      funcArgs <- getFuncArgs inpArgTyps inpArgNames
      let retTable = typeToTable returnType
          dep = SchemaDependency (SOTable retTable) "table"
      return $ SFQuery $ FunctionInfoG qf False funTy funcArgs retTable [dep]

    ccFuncPrefixMsg tn =
      "function " <> qf <<> " being tracked as computed column to " <> tn <<> " ; "

    asCompColFunc (fstArgTab, remArgs) =
      modifyErr (ccFuncPrefixMsg fstArgTab <>) $ do
        -- throw error if function returns SETOF
        when retSet $
          throw400 NotSupported "function should not return a SETOF"
        -- throw error if function does not return a scalar type
        unless (isPrimType returnType) $
          throw400 NotSupported "function does not return a scalar type"
        -- throw error if any of remaining arguments has composite type
        when (any (isJust . asTableType) remArgs) $
          throw400 NotSupported "function input argument (except first) has atleast one composite type"
        funcArgs <- getFuncArgs remArgs $ safeTail inpArgNames
        let dep = SchemaDependency (SOTable fstArgTab) "table"
        return $ SFCompCol fstArgTab $ FunctionInfoG qf False funTy funcArgs retN [dep]

    safeTail []     = []
    safeTail (_:xs) = xs

-- Build function info
getFunctionInfo :: QualifiedFunction -> Q.TxE QErr SQLFunction
getFunctionInfo qf@(QualifiedObject sn fn) = do
  -- fetch function details
  funcData <- Q.catchE defaultTxErrorHandler $
              Q.listQ $(Q.sqlFromFile "src-rsr/function_info.sql") (sn, fn) True

  case funcData of
    []                              ->
      throw400 NotExists $ "no such function exists in postgres : " <>> qf
    [Identity (Q.AltJ rawFuncInfo)] -> mkFunctionInfo qf rawFuncInfo
    _                               ->
      throw400 NotSupported $
      "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

saveFunctionToCatalog :: SQLFunction -> Bool -> Q.TxE QErr ()
saveFunctionToCatalog sqlFn isSystemDefined =
  case sqlFn of
    SFQuery qf -> saveFunction queryTy $ fiName qf
    SFCompCol tn qf -> do
      saveFunction compColTy $ fiName qf
      saveCompCol tn $ fiName qf
  where
    queryTy = "QUERY" :: T.Text
    compColTy = "COMPUTED_COLUMN" :: T.Text

    saveFunction ty (QualifiedObject sn fn) =
      Q.unitQE defaultTxErrorHandler [Q.sql|
             INSERT INTO "hdb_catalog"."hdb_function" VALUES ($1, $2, $3, $4)
                     |] (sn, fn, isSystemDefined, ty) False

    saveCompCol (QualifiedObject tsn tn) (QualifiedObject fsn fn) =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          INSERT INTO "hdb_catalog"."hdb_computed_column"
          (function_schema, function_name, table_schema, table_name)
          VALUES ($1, $2, $3, $4)
            |] (fsn, fn, tsn, tn) False

delFunctionFromCatalog :: QualifiedFunction -> Q.TxE QErr ()
delFunctionFromCatalog (QualifiedObject sn fn) =
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
                     => QualifiedFunction -> m SQLFunction
trackFunctionP2Setup qf = do
  fi <- withPathK "name" $ liftTx $ getFunctionInfo qf
  case fi of
    SFQuery f -> void $ getTI $ fiReturnType f
    SFCompCol tn f -> do
      ti <- getTI tn
      let fn = fiName f
          compColFld = FieldName $ qualObjectToText fn
      checkForFldConflict ti compColFld
  addFunctionToCache fi
  return fi
  where
    getTI tn = do
      sc <- askSchemaCache
      let err = err400 NotExists $ "table " <> tn <<> " is not tracked"
      liftMaybe err $ M.lookup tn $ scTables sc

trackFunctionP2 :: (QErrM m, CacheRWM m, MonadTx m)
                => QualifiedFunction -> m EncJSON
trackFunctionP2 qf = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
      funcNameGQL = GS.qualObjectToName qf
  -- check function name is in compliance with GraphQL spec
  unless (isValidName funcNameGQL) $ throw400 NotSupported $
    "function name " <> qf <<> " is not in compliance with GraphQL spec"
  -- check for conflicts in remote schema
  GS.checkConflictingNode defGCtx funcNameGQL
  fi <- trackFunctionP2Setup qf
  liftTx $ saveFunctionToCatalog fi False
  return successMsg

runTrackFunc
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => TrackFunction -> m EncJSON
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
  => UnTrackFunction -> m EncJSON
runUntrackFunc (UnTrackFunction qf) = do
  adminOnly
  sc <- askSchemaCache
  let deps = getDependentObjs sc $ SOFunction qf
  unless (null deps) $ reportDeps deps
  delFunctionFromCache qf
  liftTx $ delFunctionFromCatalog qf
  return successMsg
