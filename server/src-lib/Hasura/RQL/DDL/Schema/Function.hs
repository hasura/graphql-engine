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

import           Control.Lens                       hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Language.GraphQL.Draft.Syntax      as G

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types
import           Hasura.Server.Utils                (englishList, makeReasonMessage)


data RawFunctionInfo
  = RawFunctionInfo
  { rfiHasVariadic      :: !Bool
  , rfiFunctionType     :: !FunctionVolatility
  , rfiReturnTypeSchema :: !SchemaName
  , rfiReturnTypeName   :: !PGScalarType
  , rfiReturnTypeType   :: !PGTypeKind
  , rfiReturnsSet       :: !Bool
  , rfiInputArgTypes    :: ![QualifiedPGType]
  , rfiInputArgNames    :: ![FunctionArgName]
  , rfiDefaultArgs      :: !Int
  , rfiReturnsTable     :: !Bool
  , rfiDescription      :: !(Maybe PGDescription)
  } deriving (Show, Eq, Generic)
instance NFData RawFunctionInfo
instance Cacheable RawFunctionInfo
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
  | FunctionVolatilityMismatch !IntendedFunctionPosition
  | FunctionSessionArgumentNotJSON !FunctionArgName
  | FunctionInvalidSessionArgument !FunctionArgName
  | FunctionInvalidArgumentNames [FunctionArgName]
  deriving (Show, Eq)

-- | IFPMutation means the user requested @as_mutation: true@ when tracking the function.
data IntendedFunctionPosition = IFPQuery | IFPMutation 
  deriving (Show, Eq, Lift, Generic)

instance NFData IntendedFunctionPosition
instance Cacheable IntendedFunctionPosition

mkFunctionInfo
  :: (QErrM m)
  => QualifiedFunction
  -> SystemDefined
  -> FunctionConfig
  -> RawFunctionInfo
  -> m (FunctionInfo, SchemaDependency)
mkFunctionInfo qf systemDefined FunctionConfig{..} rawFuncInfo =
  either (throw400 NotSupported . showErrors) pure
    =<< MV.runValidateT validateFunction
  where
    functionArgs = mkFunctionArgs defArgsNo inpArgTyps inpArgNames
    RawFunctionInfo hasVariadic funVol retSn retN retTyTyp retSet
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
      -- Only VOLATILE functions can be exposed as mutations; after validating
      -- here we just use the volatility info in 'FunctionInfo' to decide
      -- whether it should go under the query or mutation root:
      when (funVol == FTVOLATILE && _fcIntendedPosition == IFPQuery 
         || funVol /= FTVOLATILE && _fcIntendedPosition == IFPMutation) $
        throwValidateError $ FunctionVolatilityMismatch _fcIntendedPosition

      -- validate function argument names
      validateFunctionArgNames

      inputArguments <- makeInputArguments

      let retTable = typeToTable returnType

      pure ( FunctionInfo qf systemDefined funVol inputArguments retTable descM
           , SchemaDependency (SOTable retTable) DRTable
           )

    validateFunctionArgNames = do
      let argNames = mapMaybe faName functionArgs
          invalidArgs = filter (isNothing . G.mkName . getFuncArgNameTxt) argNames
      unless (null invalidArgs) $
        throwValidateError $ FunctionInvalidArgumentNames invalidArgs

    makeInputArguments =
      case _fcSessionArgument of
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
      FunctionVolatilityMismatch IFPMutation -> 
        "only \"VOLATILE\" functions can be tracked as mutations"
      FunctionVolatilityMismatch IFPQuery  -> 
        "the function is \"VOLATILE\" (the default) and can only be tracked as a mutation, with \"as_mutation: true\""
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

data FunctionConfig
  = FunctionConfig
  { _fcSessionArgument :: !(Maybe FunctionArgName)
  -- ^ The argument, if any, to the _tfv2Function to which we'll pass the
  -- user's session variables during execution. This must be a JSON type
  -- argument, and disappears from the resulting graphql schema.
  , _fcIntendedPosition :: !IntendedFunctionPosition
  -- ^ Which top-level field the user intends for this function to end up, i.e.
  -- should it be a query or mutation?
  --
  -- We require the 'as_mutation' parameter from users in order to verify their
  -- intent; since VOLATILE is the default when creating a function, it would
  -- be easy for users to create a function then track it expecting it to be a
  -- query, only to find it ended up under the 'mutation' root field.
  --
  -- This is Maybe so we can derive instances, with Nothing implying False.
  } deriving (Show, Eq, Generic, Lift)
instance NFData FunctionConfig
instance Cacheable FunctionConfig

-- these instances written by hand to avoid a `Maybe Bool` having the semantics `False`
-- see: https://github.com/hasura/graphql-engine/pull/5858#discussion_r514100511
instance FromJSON FunctionConfig where
  parseJSON = withObject "FunctionConfig" $ \o ->
    FunctionConfig 
      <$> o .:? "session_argument"
      <*> fmap intendedFuncPos
          (o .:? "as_mutation" .!= False)

    where intendedFuncPos True  = IFPMutation
          intendedFuncPos False = IFPQuery

-- | equivalent to omitNothingFields=True:
instance ToJSON FunctionConfig where
  toJSON FunctionConfig{..} = object $
    [ "session_argument" .= argName | Just argName <- [_fcSessionArgument] ] <>
    [ "as_mutation"      .= True    | _fcIntendedPosition == IFPMutation ]
    

-- | The default function config; v1 of the API implies this.
emptyFunctionConfig :: FunctionConfig
emptyFunctionConfig = FunctionConfig Nothing IFPQuery

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

-- | JSON API payload for v2 of 'track_function':
--
-- https://hasura.io/docs/1.0/graphql/core/api-reference/schema-metadata-api/custom-functions.html#track-function-v2
data TrackFunctionV2
  = TrackFunctionV2
  { _tfv2Function      :: !QualifiedFunction
  , _tfv2Configuration :: !FunctionConfig
  } deriving (Show, Eq, Lift, Generic)
$(deriveToJSON (aesonDrop 5 snakeCase) ''TrackFunctionV2)

instance FromJSON TrackFunctionV2 where
  parseJSON = withObject "TrackFunctionV2" $ \o ->
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

-- | JSON API payload for 'untrack_function':
--
-- https://hasura.io/docs/1.0/graphql/core/api-reference/schema-metadata-api/custom-functions.html#untrack-function
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
