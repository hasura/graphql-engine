{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.Prelude

import qualified Control.Monad.Validate             as MV
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q

import           Control.Lens                       hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended

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
  | NonVolatileFunctionAsMutation
  | FunctionSessionArgumentNotJSON !FunctionArgName
  | FunctionInvalidSessionArgument !FunctionArgName
  | FunctionInvalidArgumentNames [FunctionArgName]
  deriving (Show, Eq)

mkFunctionInfo
  :: (QErrM m)
  => SourceName
  -> QualifiedFunction
  -> SystemDefined
  -> FunctionConfig
  -> RawFunctionInfo
  -> m (FunctionInfo, SchemaDependency)
mkFunctionInfo source qf systemDefined FunctionConfig{..} rawFuncInfo =
  either (throw400 NotSupported . showErrors) pure
    =<< MV.runValidateT validateFunction
  where
    functionArgs = mkFunctionArgs defArgsNo inpArgTyps inpArgNames
    RawFunctionInfo _ hasVariadic funVol retSn retN retTyTyp retSet
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
      -- We mostly take the user at their word here and will, e.g. expose a
      -- function as a query if it is marked VOLATILE (since perhaps the user
      -- is using the function to do some logging, say). But this is also a
      -- footgun we'll need to try to document (since `VOLATILE` is default
      -- when volatility is omitted). See the original approach here:
      -- https://github.com/hasura/graphql-engine/pull/5858
      --
      -- This is the one exception where we do some validation. We're not
      -- commited to this check, and it would be backwards compatible to remove
      -- it, but this seemed like an obvious case:
      when (funVol /= FTVOLATILE && _fcExposedAs == Just FEAMutation) $
        throwValidateError $ NonVolatileFunctionAsMutation
      -- If 'exposed_as' is omitted we'll infer it from the volatility:
      let exposeAs = flip fromMaybe _fcExposedAs $ case funVol of
                       FTVOLATILE -> FEAMutation
                       _          -> FEAQuery

      -- validate function argument names
      validateFunctionArgNames

      inputArguments <- makeInputArguments

      let retTable = typeToTable returnType

      pure ( FunctionInfo qf systemDefined funVol exposeAs inputArguments retTable descM
           , SchemaDependency (SOSourceObj source $ SOITable retTable) DRTable
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
      NonVolatileFunctionAsMutation ->
        "the function was requested to be exposed as a mutation, but is not marked VOLATILE. " <>
        "Maybe the function was given the wrong volatility when it was defined?"
      FunctionSessionArgumentNotJSON argName ->
        "given session argument " <> argName <<> " is not of type json"
      FunctionInvalidSessionArgument argName ->
        "given session argument " <> argName <<> " not the input argument of the function"
      FunctionInvalidArgumentNames args ->
        let argsText = T.intercalate "," $ map getFuncArgNameTxt args
        in "the function arguments " <> argsText <> " are not in compliance with GraphQL spec"

newtype TrackFunction
  = TrackFunction
  { tfName :: QualifiedFunction}
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Track function, Phase 1:
-- Validate function tracking operation. Fails if function is already being
-- tracked, or if a table with the same name is being tracked.
trackFunctionP1
  :: (CacheRM m, QErrM m) => SourceName -> QualifiedFunction -> m ()
trackFunctionP1 sourceName qf = do
  rawSchemaCache <- askSchemaCache
  when (isJust $ getPGFunctionInfo sourceName qf $ scPostgres rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf
  let qt = fmap (TableName . getFunctionTxt) qf
  when (isJust $ getPGTableInfo sourceName qt $ scPostgres rawSchemaCache) $
    throw400 NotSupported $ "table with name " <> qf <<> " already exists"

trackFunctionP2
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => SourceName -> QualifiedFunction -> FunctionConfig -> m EncJSON
trackFunctionP2 sourceName qf config = do
  buildSchemaCacheFor (MOSourceObjId sourceName $ SMOFunction qf)
    $ MetadataModifier
    $ metaSources.ix sourceName.smFunctions
      %~ OMap.insert qf (FunctionMetadata qf config)
  pure successMsg

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
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
  => TrackFunction -> m EncJSON
runTrackFunc (TrackFunction qf)= do
  -- v1 track_function lacks a means to take extra arguments
  trackFunctionP1 defaultSource qf
  trackFunctionP2 defaultSource qf emptyFunctionConfig

runTrackFunctionV2
  :: (QErrM m, CacheRWM m, MetadataM m)
  => TrackFunctionV2 -> m EncJSON
runTrackFunctionV2 (TrackFunctionV2 source qf config) = do
  trackFunctionP1 source qf
  trackFunctionP2 source qf config

-- | JSON API payload for 'untrack_function':
--
-- https://hasura.io/docs/1.0/graphql/core/api-reference/schema-metadata-api/custom-functions.html#untrack-function
data UnTrackFunction
  = UnTrackFunction
  { _utfFunction :: !QualifiedFunction
  , _utfSource   :: !SourceName
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''UnTrackFunction)

instance FromJSON UnTrackFunction where
  parseJSON v = withSource <|> withoutSource
    where
      withoutSource = UnTrackFunction <$> parseJSON v <*> pure defaultSource
      withSource = flip (withObject "Object") v \o ->
                   UnTrackFunction <$> o .: "table"
                                   <*> o .:? "source" .!= defaultSource

runUntrackFunc
  :: (CacheRWM m, MonadError QErr m, MetadataM m)
  => UnTrackFunction -> m EncJSON
runUntrackFunc (UnTrackFunction qf source) = do
  void $ askFunctionInfo source qf
  -- Delete function from metadata
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropFunctionInMetadata defaultSource qf
  pure successMsg

dropFunctionInMetadata :: SourceName -> QualifiedFunction -> MetadataModifier
dropFunctionInMetadata source function = MetadataModifier $
  metaSources.ix source.smFunctions %~ OMap.delete function
