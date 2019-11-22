{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils          (showNames)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils           (makeReasonMessage)
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Control.Monad.Validate        as MV
import qualified Data.HashMap.Strict           as M
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q

data RawFunctionInfo
  = RawFunctionInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionType
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !PGScalarType
  , rfiReturnTypeType   :: !PGTypeKind
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![QualifiedPGType]
  , rfiInputArgNames    :: ![FunctionArgName]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFunctionInfo)

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
  unless (null invalidArgs) $ throw400 NotSupported $
    "arguments: " <> showNames invalidArgs
    <> " are not in compliance with GraphQL spec"
  where
    funcArgsText = mapMaybe (fmap getFuncArgNameTxt . faName) args
    invalidArgs = filter (not . G.isValidName) $ map G.Name funcArgsText

data FunctionIntegrityError
  = FunctionVariadic
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
      -- throw error if function has variadic arguments
      when hasVariadic $ throwValidateError FunctionVariadic
      -- throw error if return type is not composite type
      when (retTyTyp /= PGKindComposite) $ throwValidateError FunctionReturnNotCompositeType
      -- throw error if function do not returns SETOF
      unless retSet $ throwValidateError FunctionReturnNotSetof
      -- throw error if return type is not a valid table
      unless returnsTab $ throwValidateError FunctionReturnNotSetofTable
      -- throw error if function type is VOLATILE
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
          invalidArgs = filter (not . G.isValidName . G.Name . getFuncArgNameTxt) argNames
      when (not $ null invalidArgs) $
        throwValidateError $ FunctionInvalidArgumentNames invalidArgs

    makeInputArguments =
      case _fcSessionArgument config of
        Nothing -> pure $ Seq.fromList $ map IAUserProvided functionArgs
        Just sessionArgName -> do
          when (not $ any (\arg -> (Just sessionArgName) == faName arg) functionArgs) $
            throwValidateError $ FunctionInvalidSessionArgument sessionArgName
          fmap Seq.fromList $ forM functionArgs $ \arg ->
            if (Just sessionArgName) == faName arg then do
              let argTy = _qptName $ faType arg
              if argTy == PGJSON then pure $ IASessionVariables sessionArgName
              else MV.refute $ pure $ FunctionSessionArgumentNotJSON sessionArgName
            else pure $ IAUserProvided arg

    showErrors allErrors =
      "the function " <> qf <<> " cannot be tracked "
      <> makeReasonMessage allErrors showOneError

    showOneError = \case
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

saveFunctionToCatalog :: QualifiedFunction -> FunctionConfig -> SystemDefined -> Q.TxE QErr ()
saveFunctionToCatalog (QualifiedObject sn fn) config systemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
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

data FunctionConfig
  = FunctionConfig
  { _fcSessionArgument :: !(Maybe FunctionArgName)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields = True} ''FunctionConfig)

emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing

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

trackFunctionP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedFunction -> SystemDefined -> FunctionConfig -> RawFunctionInfo -> m ()
trackFunctionP2Setup qf systemDefined config rawfi = do
  (fi, dep) <- mkFunctionInfo qf systemDefined config rawfi
  let retTable = fiReturnType fi
      err = err400 NotExists $ "table " <> retTable <<> " is not tracked"
  sc <- askSchemaCache
  void $ liftMaybe err $ M.lookup retTable $ scTables sc
  addFunctionToCache fi [dep]

trackFunctionP2 :: (QErrM m, CacheRWM m, HasSystemDefined m, MonadTx m)
                => QualifiedFunction -> FunctionConfig -> m EncJSON
trackFunctionP2 qf config = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
      funcNameGQL = GS.qualObjectToName qf
  -- check function name is in compliance with GraphQL spec
  unless (G.isValidName funcNameGQL) $ throw400 NotSupported $
    "function name " <> qf <<> " is not in compliance with GraphQL spec"
  -- check for conflicts in remote schema
  GS.checkConflictingNode defGCtx funcNameGQL

  -- fetch function info
  rawfi <- fetchRawFunctioInfo qf
  systemDefined <- askSystemDefined
  trackFunctionP2Setup qf systemDefined config rawfi
  liftTx $ saveFunctionToCatalog qf config systemDefined
  return successMsg

handleMultipleFunctions :: (QErrM m) => QualifiedFunction -> [a] -> m a
handleMultipleFunctions qf = \case
  []      ->
    throw400 NotExists $ "no such function exists in postgres : " <>> qf
  [fi] -> return fi
  _       ->
    throw400 NotSupported $
    "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

fetchRawFunctioInfo :: MonadTx m => QualifiedFunction -> m RawFunctionInfo
fetchRawFunctioInfo qf@(QualifiedObject sn fn) =
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
  :: ( QErrM m
     , CacheRWM m
     , HasSystemDefined m
     , MonadTx m
     )
  => TrackFunction -> m EncJSON
runTrackFunc (TrackFunction qf)= do
  trackFunctionP1 qf
  trackFunctionP2 qf emptyFunctionConfig

data TrackFunctionV2
  = TrackFunctionV2
  { _tfv2Function      :: !QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TrackFunctionV2)

instance FromJSON TrackFunctionV2 where
  parseJSON = withObject "Object" $ \o ->
    TrackFunctionV2
    <$> o .: "function"
    <*> o .:? "configuration" .!= emptyFunctionConfig

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
  delFunctionFromCache qf
  return successMsg
