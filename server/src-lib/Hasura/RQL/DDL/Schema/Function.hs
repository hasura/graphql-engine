module Hasura.RQL.DDL.Schema.Function where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils          (isValidName, showNames)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Foldable                 (foldlM)
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

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
  , rfiReturnsTable     :: !Bool
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''RawFuncInfo)

mkFunctionArgs :: [PGColType] -> [T.Text] -> [FunctionArg]
mkFunctionArgs tys argNames = map mkInpFuncArgs $
  bool withNames withNoNames $ null argNames
  where
    mkInpFuncArgs (mName, ty) = FunctionArg mName ty FATInput

    withNoNames = flip map tys $ \ty -> (Nothing, ty)
    withNames = zipWith mkArg argNames tys

    mkArg "" ty = (Nothing, ty)
    mkArg n  ty = (Just $ FunctionArgName n, ty)

validateFuncArgs :: MonadError QErr m => [FunctionArg] -> m ()
validateFuncArgs args =
  unless (null invalidArgs) $ throw400 NotSupported $
    "arguments: " <> showNames invalidArgs
    <> " are not in compliance with GraphQL spec"
  where
    funcArgsText = mapMaybe (fmap getFuncArgNameTxt . faName) args
    invalidArgs = filter (not . isValidName) $ map G.Name funcArgsText

validateSessionArg
  :: QErrM m
  => [FunctionArg] -> FunctionArgName -> m [FunctionArg]
validateSessionArg funcArgs sessArg =
  withPathK "session_variable_argument" $ do

  (modFuncArgs, isModified) <- foldlM go ([], False) funcArgs

  when (not isModified) $ throw400 NotFound $
    "function argument with name " <> sessArg <<> " not found"

  return modFuncArgs
  where
    go (argList, isMod) arg =
      if faName arg == Just sessArg then do
        when (not $ isJSONType $ faColType arg) $
          throw400 NotSupported
          "session variable argument should be of type JSON"
        return (argList <> pure arg{faArgType = FATSession}, isMod || True)
      else return (argList <> pure arg, isMod || False)

mkFunctionInfo
  :: QErrM m
  => QualifiedFunction
  -> Maybe FunctionConfig
  -> RawFuncInfo
  -> m FunctionInfo
mkFunctionInfo qf mConfig rawFuncInfo = do
  -- throw error if function has variadic arguments
  when hasVariadic $ throw400 NotSupported "function with \"VARIADIC\" parameters are not supported"
  -- throw error if return type is not composite type
  when (retTyTyp /= PTCOMPOSITE) $ throw400 NotSupported "function does not return a \"COMPOSITE\" type"
  -- throw error if function do not returns SETOF
  unless retSet $ throw400 NotSupported "function does not return a SETOF"
  -- throw error if return type is not a valid table
  unless returnsTab $ throw400 NotSupported "function does not return a SETOF table"
  -- throw error if function type is VOLATILE
  when (funTy == FTVOLATILE) $ throw400 NotSupported "function of type \"VOLATILE\" is not supported now"

  let funcInpArgs = mkFunctionArgs inpArgTyps inpArgNames
  validateFuncArgs funcInpArgs

  -- modify argument type of session argument with validation
  mFuncArgs <- withPathK "config" $
                 forM mSessArg $ validateSessionArg funcInpArgs

  let funcArgsSeq = Seq.fromList $ fromMaybe funcInpArgs mFuncArgs
      dep = SchemaDependency (SOTable retTable) "table"
      retTable = QualifiedObject retSn (TableName retN)
  return $ FunctionInfo qf False funTy funcArgsSeq retTable [dep]
  where
    mSessArg = _fcSessionVariableArgument <$> mConfig
    RawFuncInfo hasVariadic funTy retSn retN retTyTyp
                retSet inpArgTyps inpArgNames returnsTab
                = rawFuncInfo

saveFunctionToCatalog
  :: QualifiedFunction
  -> Maybe FunctionConfig
  -> Bool
  -> Q.TxE QErr ()
saveFunctionToCatalog (QualifiedObject sn fn) mConfig isSystemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
         INSERT INTO "hdb_catalog"."hdb_function"
           (function_schema, function_name,
            config, is_system_defined
           ) VALUES ($1, $2, $3, $4)
        |] (sn, fn, (Q.AltJ . toJSON) <$> mConfig , isSystemDefined) True

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

newtype FunctionConfig
  = FunctionConfig { _fcSessionVariableArgument :: FunctionArgName}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''FunctionConfig)

data TrackFunctionV2
  = TrackFunctionV2
  { tfv2Function :: !QualifiedFunction
  , tfv2Config   :: !(Maybe FunctionConfig)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''TrackFunctionV2)

trackFunctionP1
  :: (CacheRM m, UserInfoM m, QErrM m) => QualifiedFunction -> m ()
trackFunctionP1 qf = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf

trackFunctionP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedFunction
  -> Maybe FunctionConfig
  -> RawFuncInfo -> m ()
trackFunctionP2Setup qf mConfig rawfi = do
  fi <- mkFunctionInfo qf mConfig rawfi
  let retTable = fiReturnType fi
      err = err400 NotExists $ "table " <> retTable <<> " is not tracked"
  sc <- askSchemaCache
  void $ liftMaybe err $ M.lookup retTable $ scTables sc
  addFunctionToCache fi

trackFunctionP2 :: (QErrM m, CacheRWM m, MonadTx m)
                => QualifiedFunction -> Maybe FunctionConfig -> m EncJSON
trackFunctionP2 qf mConfig = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
      funcNameGQL = GS.qualObjectToName qf
  -- check function name is in compliance with GraphQL spec
  unless (isValidName funcNameGQL) $ throw400 NotSupported $
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
      throw400 NotSupported $ "function "
      <> qf <<> " is overloaded. Overloaded functions are not supported"
  trackFunctionP2Setup qf mConfig rawfi
  liftTx $ saveFunctionToCatalog qf mConfig False
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
  :: ( CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => TrackFunction -> m EncJSON
runTrackFunc (TrackFunction qf) =
  runTrackFuncG qf Nothing

runTrackFuncV2
  :: ( CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => TrackFunctionV2 -> m EncJSON
runTrackFuncV2 (TrackFunctionV2 qf mConfig) =
  runTrackFuncG qf mConfig

runTrackFuncG
  :: ( QErrM m, CacheRWM m, MonadTx m
     , UserInfoM m
     )
  => QualifiedFunction -> Maybe FunctionConfig -> m EncJSON
runTrackFuncG qf mConfig = do
  trackFunctionP1 qf
  trackFunctionP2 qf mConfig

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

fetchFunctions :: Q.TxE QErr [TrackFunctionV2]
fetchFunctions =
  map (Q.getAltJ . runIdentity)
    <$> Q.listQE defaultTxErrorHandler
      [Q.sql|
         SELECT
           json_build_object(
             'function',
             json_build_object(
               'schema', hf.function_schema,
               'name', hf.function_name
             ),
             'config', hf.config
           ) as function
         FROM hdb_catalog.hdb_function hf
      |] () True
