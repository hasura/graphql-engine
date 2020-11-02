{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.Prelude

import qualified Control.Monad.Validate             as MV
import qualified Data.HashMap.Strict                as M
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q

import           Control.Lens
import           Data.Aeson
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Language.GraphQL.Draft.Syntax      as G

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Server.Utils                (englishList, makeReasonMessage)


mkFunctionArgs :: Int -> [QualifiedPGType] -> [FunctionArgName] -> [FunctionArg]
mkFunctionArgs defArgsNo tys argNames =
  bool withNames withNoNames $ null argNames
  where
    hasDefaultBoolSeq = replicate (length tys - defArgsNo) (HasDefault False)
                        -- only last arguments can have default expression
                        <> replicate defArgsNo (HasDefault True)

    tysWithHasDefault = zip tys hasDefaultBoolSeq

    withNoNames = flip map tysWithHasDefault $ uncurry $ FunctionArg Nothing
    withNames = zipWith mkArg argNames tysWithHasDefault

    mkArg "" (ty, hasDef) = FunctionArg Nothing ty hasDef
    mkArg n  (ty, hasDef) = FunctionArg (Just n) ty hasDef

validateFuncArgs :: MonadError QErr m => [FunctionArg] -> m ()
validateFuncArgs args =
  for_ (nonEmpty invalidArgs) \someInvalidArgs ->
    throw400 NotSupported $
      "arguments: " <> englishList "and" someInvalidArgs
      <> " are not in compliance with GraphQL spec"
  where
    funcArgsText = mapMaybe (fmap getFuncArgNameTxt . faName) args
    invalidArgs = filter (isNothing . G.mkName) funcArgsText

data FunctionIntegrityError
  = FunctionNameNotGQLCompliant
  | FunctionVariadic
  | FunctionReturnNotCompositeType
  | FunctionReturnNotSetof
  | FunctionReturnNotSetofTable
  | FunctionVolatile
  | FunctionSessionArgumentNotJSON !FunctionArgName
  | FunctionInvalidSessionArgument !FunctionArgName
  | FunctionInvalidArgumentNames [FunctionArgName]
  deriving (Show, Eq)

mkFunctionInfo
  :: (QErrM m)
  => QualifiedFunction
  -> SystemDefined
  -> FunctionConfig
  -> RawFunctionInfo
  -> m (FunctionInfo, SchemaDependency)
mkFunctionInfo qf systemDefined config rawFuncInfo =
  either (throw400 NotSupported . showErrors) pure
    =<< MV.runValidateT validateFunction
  where
    functionArgs = mkFunctionArgs defArgsNo inpArgTyps inpArgNames
    RawFunctionInfo hasVariadic funTy retSn retN retTyTyp retSet
                inpArgTyps inpArgNames defArgsNo returnsTab descM
                = rawFuncInfo
    returnType = QualifiedPGType retSn retN retTyTyp

    throwValidateError = MV.dispute . pure

    validateFunction = do
      unless (has _Right $ qualifiedObjectToName qf) $
        throwValidateError FunctionNameNotGQLCompliant
      when hasVariadic $ throwValidateError FunctionVariadic
      when (retTyTyp /= PGKindComposite) $ throwValidateError FunctionReturnNotCompositeType
      unless retSet $ throwValidateError FunctionReturnNotSetof
      unless returnsTab $ throwValidateError FunctionReturnNotSetofTable
      when (funTy == FTVOLATILE) $ throwValidateError FunctionVolatile

      -- validate function argument names
      validateFunctionArgNames

      inputArguments <- makeInputArguments

      let retTable = typeToTable returnType

      pure ( FunctionInfo qf systemDefined funTy inputArguments retTable descM
           , SchemaDependency (SOTable retTable) DRTable
           )

    validateFunctionArgNames = do
      let argNames = mapMaybe faName functionArgs
          invalidArgs = filter (isNothing . G.mkName . getFuncArgNameTxt) argNames
      unless (null invalidArgs) $
        throwValidateError $ FunctionInvalidArgumentNames invalidArgs

    makeInputArguments =
      case _fcSessionArgument config of
        Nothing -> pure $ Seq.fromList $ map IAUserProvided functionArgs
        Just sessionArgName -> do
          unless (any (\arg -> Just sessionArgName == faName arg) functionArgs) $
            throwValidateError $ FunctionInvalidSessionArgument sessionArgName
          fmap Seq.fromList $ forM functionArgs $ \arg ->
            if Just sessionArgName == faName arg then do
              let argTy = _qptName $ faType arg
              if argTy == PGJSON then pure $ IASessionVariables sessionArgName
              else MV.refute $ pure $ FunctionSessionArgumentNotJSON sessionArgName
            else pure $ IAUserProvided arg

    showErrors allErrors =
      "the function " <> qf <<> " cannot be tracked "
      <> makeReasonMessage allErrors showOneError

    showOneError = \case
      FunctionNameNotGQLCompliant -> "function name is not a legal GraphQL identifier"
      FunctionVariadic -> "function with \"VARIADIC\" parameters are not supported"
      FunctionReturnNotCompositeType -> "the function does not return a \"COMPOSITE\" type"
      FunctionReturnNotSetof -> "the function does not return a SETOF"
      FunctionReturnNotSetofTable -> "the function does not return a SETOF table"
      FunctionVolatile -> "function of type \"VOLATILE\" is not supported now"
      FunctionSessionArgumentNotJSON argName ->
        "given session argument " <> argName <<> " is not of type json"
      FunctionInvalidSessionArgument argName ->
        "given session argument " <> argName <<> " not the input argument of the function"
      FunctionInvalidArgumentNames args ->
        let argsText = T.intercalate "," $ map getFuncArgNameTxt args
        in "the function arguments " <> argsText <> " are not in compliance with GraphQL spec"

saveFunctionToCatalog
  :: (MonadTx m, HasSystemDefined m)
  => QualifiedFunction -> FunctionConfig -> m ()
saveFunctionToCatalog (QualifiedObject sn fn) config = do
  systemDefined <- askSystemDefined
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function"
           (function_schema, function_name, configuration, is_system_defined)
         VALUES ($1, $2, $3, $4)
                 |] (sn, fn, Q.AltJ config, systemDefined) False

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

-- | Track function, Phase 1:
-- Validate function tracking operation. Fails if function is already being
-- tracked, or if a table with the same name is being tracked.
trackFunctionP1
  :: (CacheRM m, QErrM m) => QualifiedFunction -> m ()
trackFunctionP1 qf = do
  rawSchemaCache <- askSchemaCache
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf
  let qt = fmap (TableName . getFunctionTxt) qf
  when (M.member qt $ scTables rawSchemaCache) $
    throw400 NotSupported $ "table with name " <> qf <<> " already exists"

trackFunctionP2 :: (MonadTx m, CacheRWM m, HasSystemDefined m)
                => QualifiedFunction -> FunctionConfig -> m EncJSON
trackFunctionP2 qf config = do
  saveFunctionToCatalog qf config
  buildSchemaCacheFor $ MOFunction qf
  return successMsg

handleMultipleFunctions :: (QErrM m) => QualifiedFunction -> [a] -> m a
handleMultipleFunctions qf = \case
  []      ->
    throw400 NotExists $ "no such function exists in postgres : " <>> qf
  [fi] -> return fi
  _       ->
    throw400 NotSupported $
    "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

fetchRawFunctionInfo :: MonadTx m => QualifiedFunction -> m RawFunctionInfo
fetchRawFunctionInfo qf@(QualifiedObject sn fn) =
  handleMultipleFunctions qf =<< map (Q.getAltJ . runIdentity) <$> fetchFromDatabase
  where
    fetchFromDatabase = liftTx $
      Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT function_info
             FROM hdb_catalog.hdb_function_info_agg
            WHERE function_schema = $1
              AND function_name = $2
          |] (sn, fn) True

runTrackFunc
  :: (MonadTx m, CacheRWM m, HasSystemDefined m)
  => TrackFunction -> m EncJSON
runTrackFunc (TrackFunction qf)= do
  trackFunctionP1 qf
  trackFunctionP2 qf emptyFunctionConfig

runTrackFunctionV2
  :: ( QErrM m, CacheRWM m, HasSystemDefined m
     , MonadTx m
     )
  => TrackFunctionV2 -> m EncJSON
runTrackFunctionV2 (TrackFunctionV2 qf config) = do
  trackFunctionP1 qf
  trackFunctionP2 qf config

newtype UnTrackFunction
  = UnTrackFunction
  { utfName :: QualifiedFunction }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

runUntrackFunc
  :: (QErrM m, CacheRWM m, MonadTx m)
  => UnTrackFunction -> m EncJSON
runUntrackFunc (UnTrackFunction qf) = do
  void $ askFunctionInfo qf
  liftTx $ delFunctionFromCatalog qf
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg
