module Hasura.Backends.Postgres.DDL.Function
  ( buildFunctionInfo
  , mkFunctionArgs
  )
where

import           Hasura.Prelude

import qualified Control.Monad.Validate             as MV
import qualified Data.HashSet                       as S
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Lens                       hiding (from, index, op, (.=))
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Backend
import           Hasura.Server.Utils

mkFunctionArgs :: Int -> [QualifiedPGType] -> [FunctionArgName] -> [FunctionArg 'Postgres]
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

data FunctionIntegrityError
  = FunctionNameNotGQLCompliant
  | FunctionVariadic
  | FunctionReturnNotCompositeType
  | FunctionReturnNotTable
  | NonVolatileFunctionAsMutation
  | FunctionSessionArgumentNotJSON !FunctionArgName
  | FunctionInvalidSessionArgument !FunctionArgName
  | FunctionInvalidArgumentNames [FunctionArgName]
  | FunctionInvalidArgumentType ![QualifiedPGType]
  deriving (Show, Eq)

buildFunctionInfo
  :: (QErrM m)
  => SourceName
  -> QualifiedFunction
  -> SystemDefined
  -> FunctionConfig
  -> [FunctionPermissionMetadata]
  -> RawFunctionInfo
  -> m (FunctionInfo 'Postgres, SchemaDependency)
buildFunctionInfo source qf systemDefined FunctionConfig{..} permissions rawFuncInfo =
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
      unless returnsTab $ throwValidateError FunctionReturnNotTable
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
        throwValidateError NonVolatileFunctionAsMutation
      -- If 'exposed_as' is omitted we'll infer it from the volatility:
      let exposeAs = flip fromMaybe _fcExposedAs $ case funVol of
                       FTVOLATILE -> FEAMutation
                       _          -> FEAQuery

      -- validate function argument names
      validateFunctionArgNames

      -- validate function argument types
      validateFunctionArgTypes

      inputArguments <- makeInputArguments

      let retTable = typeToTable returnType
          retJsonAggSelect = bool JASSingleObject JASMultipleRows retSet
          functionInfo =
            FunctionInfo qf systemDefined funVol exposeAs inputArguments
                         retTable descM (S.fromList $ _fpmRole <$> permissions)
                         retJsonAggSelect

      pure ( functionInfo
           , SchemaDependency
               (SOSourceObj source
                 $ AB.mkAnyBackend
                 $ SOITable retTable)
               DRTable
           )

    validateFunctionArgNames = do
      let argNames = mapMaybe faName functionArgs
          invalidArgs = filter (isNothing . G.mkName . getFuncArgNameTxt) argNames
      unless (null invalidArgs) $
        throwValidateError $ FunctionInvalidArgumentNames invalidArgs

    validateFunctionArgTypes = do
      let nonBaseTypeArgs = filter (not . isBaseType) inpArgTyps
      unless (null nonBaseTypeArgs) $
        throwValidateError $ FunctionInvalidArgumentType nonBaseTypeArgs

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
      FunctionReturnNotTable -> "the function does not return a table"
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
      FunctionInvalidArgumentType argTypes ->
        let argsText = T.intercalate "," $ map (toTxt . _qptName) argTypes
        in "the function argument types "
           <> argsText
           <<> " are not supported, because function arguments are expected"
           <> " to be of the type: 'base'"
