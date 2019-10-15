{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils          (showNames)
import           Hasura.Prelude
import           Hasura.RQL.Types
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


data PGTypType
  = PTBASE
  | PTCOMPOSITE
  | PTDOMAIN
  | PTENUM
  | PTRANGE
  | PTPSEUDO
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
  , rfiInputArgTypes    :: ![PGScalarType]
  , rfiInputArgNames    :: ![T.Text]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFuncInfo)

mkFunctionArgs :: Int -> [PGScalarType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs defArgsNo tys argNames =
  bool withNames withNoNames $ null argNames
  where
    hasDefaultBoolSeq = replicate (length argNames - defArgsNo) False
                        -- only last arguments can have default expression
                        <> replicate defArgsNo True

    tysWithHasDefault = zip tys hasDefaultBoolSeq

    withNoNames = flip map tysWithHasDefault $ uncurry $ FunctionArg Nothing
    withNames = zipWith mkArg argNames tysWithHasDefault

    mkArg "" (ty, hasDef) = FunctionArg Nothing ty hasDef
    mkArg n  (ty, hasDef) = FunctionArg (Just $ FunctionArgName n) ty hasDef

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
  :: (QErrM m, HasSystemDefined m)
  => QualifiedFunction -> FunctionConfig -> RawFuncInfo -> m FunctionInfo
mkFunctionInfo qf config rawFuncInfo = do
  systemDefined <- askSystemDefined
  either (throw400 NotSupported . showErrors) pure
    =<< MV.runValidateT (validateFunction systemDefined)
  where
    functionArgs = mkFunctionArgs defArgsNo inpArgTyps inpArgNames
    RawFuncInfo hasVariadic funTy retSn retN retTyTyp retSet
                inpArgTyps inpArgNames defArgsNo returnsTab descM
                = rawFuncInfo

    throwValidateError = MV.dispute . pure

    validateFunction systemDefined = do
      -- throw error if function has variadic arguments
      when hasVariadic $ throwValidateError FunctionVariadic
      -- throw error if return type is not composite type
      when (retTyTyp /= PTCOMPOSITE) $ throwValidateError FunctionReturnNotCompositeType
      -- throw error if function do not returns SETOF
      unless retSet $ throwValidateError FunctionReturnNotSetof
      -- throw error if return type is not a valid table
      unless returnsTab $ throwValidateError FunctionReturnNotSetofTable
      -- throw error if function type is VOLATILE
      when (funTy == FTVOLATILE) $ throwValidateError FunctionVolatile

      -- validate function argument names
      validateFunctionArgNames

      maybeSessArg <- resolveSessionArgument

      let funcArgsSeq = Seq.fromList functionArgs
          dep = SchemaDependency (SOTable retTable) DRTable
          retTable = QualifiedObject retSn (TableName retN)
      return $ FunctionInfo qf systemDefined funTy funcArgsSeq maybeSessArg retTable [dep] descM

    validateFunctionArgNames = do
      let argNames = mapMaybe faName functionArgs
          invalidArgs = filter (not . G.isValidName . G.Name . getFuncArgNameTxt) argNames
      when (not $ null invalidArgs) $
        throwValidateError $ FunctionInvalidArgumentNames invalidArgs

    resolveSessionArgument =
      forM (_fcSessionArgument config) $ \argName ->
        case findWithIndex (maybe False (argName ==) . faName) functionArgs of
          Nothing -> MV.refute $ pure $ FunctionInvalidSessionArgument argName
          Just (arg, index) -> do
            let ty = faType arg
            when (ty /= PGJSON) $ throwValidateError $ FunctionSessionArgumentNotJSON argName
            pure $ SessionArgument argName index

    showErrors allErrors =
      let reasonMessage = case allErrors of
            [singleError] -> "because " <> showOneError singleError
            _ -> "for the following reasons:\n" <> T.unlines
              (map (("  • " <>) . showOneError) allErrors)
      in "the function " <> qf <<> " cannot be tracked " <> reasonMessage

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
  :: (CacheRM m, UserInfoM m, QErrM m) => QualifiedFunction -> m ()
trackFunctionP1 qf = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf
  let qt = fmap (TableName . getFunctionTxt) qf
  when (M.member qt $ scTables rawSchemaCache) $
    throw400 NotSupported $ "table with name " <> qf <<> " already exists"

trackFunctionP2Setup :: (QErrM m, CacheRWM m, HasSystemDefined m, MonadTx m)
                     => QualifiedFunction -> FunctionConfig -> RawFuncInfo -> m ()
trackFunctionP2Setup qf config rawfi = do
  fi <- mkFunctionInfo qf config rawfi
  let retTable = fiReturnType fi
      err = err400 NotExists $ "table " <> retTable <<> " is not tracked"
  sc <- askSchemaCache
  void $ liftMaybe err $ M.lookup retTable $ scTables sc
  addFunctionToCache fi

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
  functionInfos <- liftTx fetchFuncDets
  rawfi <- case functionInfos of
    []      ->
      throw400 NotExists $ "no such function exists in postgres : " <>> qf
    [rawfi] -> return rawfi
    _       ->
      throw400 NotSupported $
      "function " <> qf <<> " is overloaded. Overloaded functions are not supported"
  trackFunctionP2Setup qf config rawfi
  systemDefined <- askSystemDefined
  liftTx $ saveFunctionToCatalog qf config systemDefined
  return successMsg
  where
    QualifiedObject sn fn = qf
    fetchFuncDets = map (Q.getAltJ . runIdentity) <$>
      Q.listQE defaultTxErrorHandler [Q.sql|
            SELECT function_info
              FROM hdb_catalog.hdb_function_info_agg
             WHERE function_schema = $1
               AND function_name = $2
           |] (sn, fn) True

runTrackFunc
  :: ( QErrM m, CacheRWM m, HasSystemDefined m
     , MonadTx m, UserInfoM m
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
     , MonadTx m, UserInfoM m
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
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => UnTrackFunction -> m EncJSON
runUntrackFunc (UnTrackFunction qf) = do
  adminOnly
  void $ askFunctionInfo qf
  liftTx $ delFunctionFromCatalog qf
  delFunctionFromCache qf
  return successMsg
