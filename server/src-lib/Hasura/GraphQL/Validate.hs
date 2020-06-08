module Hasura.GraphQL.Validate
  ( validateGQ
  , showVars
  , RootSelectionSet(..)
  , SelectionSet(..)
  , Field(..)
  , getTypedOp
  , QueryParts(..)
  , getQueryParts

  , ReusableVariableTypes(..)
  , ReusableVariableValues
  , validateVariablesForReuse

  , isQueryInAllowlist
  , unValidateArgsMap
  , unValidateSelectionSet
  , unValidateField
  ) where

import           Hasura.Prelude

import           Data.Has

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as HS
import qualified Data.Sequence                          as Seq
import qualified Data.Text                              as T
import qualified Data.UUID                              as UUID
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.Resolve.InputValue      (annInpValueToJson)
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.SelectionSet
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection
import           Hasura.SQL.Time
import           Hasura.SQL.Value

data QueryParts
  = QueryParts
  { qpOpDef     :: !G.TypedOperationDefinition
  , qpOpRoot    :: !ObjTyInfo
  , qpFragDefsL :: ![G.FragmentDefinition]
  , qpVarValsM  :: !(Maybe VariableValues)
  } deriving (Show, Eq)

getTypedOp
  :: (MonadError QErr m)
  => Maybe OperationName
  -> [G.SelectionSet]
  -> [G.TypedOperationDefinition]
  -> m G.TypedOperationDefinition
getTypedOp opNameM selSets opDefs =
  case (opNameM, selSets, opDefs) of
    (Just opName, [], _) -> do
      let n = _unOperationName opName
          opDefM = find (\opDef -> G._todName opDef == Just n) opDefs
      onNothing opDefM $ throwVE $
        "no such operation found in the document: " <> showName n
    (Just _, _, _)  ->
      throwVE $ "operationName cannot be used when " <>
      "an anonymous operation exists in the document"
    (Nothing, [selSet], []) ->
      return $ G.TypedOperationDefinition
      G.OperationTypeQuery Nothing [] [] selSet
    (Nothing, [], [opDef])  ->
      return opDef
    (Nothing, _, _) ->
      throwVE $ "exactly one operation has to be present " <>
      "in the document when operationName is not specified"

-- | For all the variables defined there will be a value in the final map
-- If no default, not in variables and nullable, then null value
validateVariables
  :: (MonadReader r m, Has TypeMap r, MonadError QErr m)
  => [G.VariableDefinition] -> VariableValues -> m AnnVarVals
validateVariables varDefsL inpVals = withPathK "variableValues" $ do
  varDefs <- onLeft (mkMapWith G._vdVariable varDefsL) $ \dups ->
    throwVE $ "the following variables are defined more than once: " <>
    showVars dups

  let unexpectedVars = filter (not . (`Map.member` varDefs)) $ Map.keys inpVals
  unless (null unexpectedVars) $
    throwVE $ "unexpected variables in variableValues: " <>
    showVars unexpectedVars

  traverse validateVariable varDefs
  where
    validateVariable (G.VariableDefinition var ty defM) = do
      let baseTy = getBaseTy ty
      baseTyInfo <- getTyInfoVE baseTy
      -- check that the variable is defined on input types
      when (isObjTy baseTyInfo) $ throwVE $
        "variables can only be defined on input types"
        <> "(enums, scalars, input objects), but "
        <> showNamedTy baseTy <> " is an object type"

      let defM' = bool (defM <|> Just G.VCNull) defM $ G.isNotNull ty
      annDefM <- withPathK "defaultValue" $
                 mapM (validateInputValue constValueParser ty) defM'
      let inpValM = Map.lookup var inpVals
      annInpValM <- withPathK (G.unName $ G.unVariable var) $
                    mapM (validateInputValue jsonParser ty) inpValM
      let varValM = annInpValM <|> annDefM
      onNothing varValM $ throwVE $
        "expecting a value for non-nullable variable: " <>
        showVars [var] <>
        " of type: " <> G.showGT ty <>
        " in variableValues"


showVars :: (Functor f, Foldable f) => f G.Variable -> Text
showVars = showNames . fmap G.unVariable

-- | This is similar in spirit to 'validateVariables' but uses preexisting 'ReusableVariableTypes'
-- information to parse Postgres values directly for use with a reusable query plan. (Ideally, it
-- would be nice to be able to share more of the logic instead of duplicating it.)
validateVariablesForReuse
  :: (MonadError QErr m)
  => ReusableVariableTypes -> Maybe VariableValues -> m ReusableVariableValues
validateVariablesForReuse (ReusableVariableTypes varTypes) varValsM =
  withPathK "variableValues" $ do
    let unexpectedVars = filter (not . (`Map.member` varTypes)) $ Map.keys varVals
    unless (null unexpectedVars) $
      throwVE $ "unexpected variables: " <> showVars unexpectedVars

    flip Map.traverseWithKey varTypes $ \varName varType ->
      withPathK (G.unName $ G.unVariable varName) $ do
        varVal <- onNothing (Map.lookup varName varVals) $
          throwVE "expected a value for non-nullable variable"
          -- TODO: we don't have the graphql type
          -- <> " of type: " <> T.pack (show varType)
        parsePGScalarValue varType varVal
    where
      varVals = fromMaybe Map.empty varValsM

validateFrag
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => G.FragmentDefinition -> m FragDef
validateFrag (G.FragmentDefinition n onTy dirs selSet) = do
  unless (null dirs) $ throwVE
    "unexpected directives at fragment definition"
  fragmentTypeInfo <- getFragmentTyInfo onTy
  return $ FragDef n fragmentTypeInfo selSet

validateGQ
  :: (MonadError QErr m, MonadReader GCtx m, MonadReusability m)
  => QueryParts -> m RootSelectionSet
validateGQ (QueryParts opDef opRoot fragDefsL varValsM) = do
  ctx <- ask

  -- annotate the variables of this operation
  annVarVals <- validateVariables (G._todVariableDefinitions opDef) $ fromMaybe Map.empty varValsM

  -- annotate the fragments
  fragDefs <- onLeft (mkMapWith G._fdName fragDefsL) $ \dups ->
    throwVE $ "the following fragments are defined more than once: " <>
    showNames dups
  annFragDefs <- mapM validateFrag fragDefs

  -- build a validation ctx
  let valCtx = ValidationCtx (_gTypes ctx) annVarVals annFragDefs

  selSet <- flip runReaderT valCtx $ parseObjectSelectionSet valCtx opRoot $
            G._todSelectionSet opDef

  case G._todType opDef of
    G.OperationTypeQuery -> return $ RQuery selSet
    G.OperationTypeMutation -> return $ RMutation selSet
    G.OperationTypeSubscription ->
      case OMap.toList $ unAliasedFields $ unObjectSelectionSet selSet of
        []             -> throw500 "empty selset for subscription"
        (_:rst) -> do
          -- As an internal testing feature, we support subscribing to multiple
          -- selection sets.  First check if the corresponding directive is set.
          let multipleAllowed = G.Directive "_multiple_top_level_fields" [] `elem` G._todDirectives opDef
          unless (multipleAllowed || null rst) $
            throwVE "subscriptions must select one top level field"
          return $ RSubscription selSet

isQueryInAllowlist :: GQLExecDoc -> HS.HashSet GQLQuery -> Bool
isQueryInAllowlist q = HS.member gqlQuery
  where
    gqlQuery = GQLQuery $ G.ExecutableDocument $ stripTypenames $
               unGQLExecDoc q

getQueryParts
  :: ( MonadError QErr m, MonadReader GCtx m)
  => GQLReqParsed
  -> m QueryParts
getQueryParts (GQLReq opNameM q varValsM) = do
  -- get the operation that needs to be evaluated
  opDef <- getTypedOp opNameM selSets opDefs
  ctx <- ask

  -- get the operation root
  opRoot <- case G._todType opDef of
    G.OperationTypeQuery        -> return $ _gQueryRoot ctx
    G.OperationTypeMutation     ->
      onNothing (_gMutRoot ctx) $ throwVE "no mutations exist"
    G.OperationTypeSubscription ->
      onNothing (_gSubRoot ctx) $ throwVE "no subscriptions exist"
  return $ QueryParts opDef opRoot fragDefsL varValsM
  where
    (selSets, opDefs, fragDefsL) = G.partitionExDefs $ unGQLExecDoc q

-- | Convert the validated arguments to GraphQL parser AST arguments
unValidateArgsMap :: ArgsMap -> [RemoteFieldArgument]
unValidateArgsMap argsMap =
  map (\(n, inpVal) ->
         let _rfaArgument = G.Argument n $ unValidateInpVal inpVal
             _rfaVariable = unValidateInpVariable inpVal
         in RemoteFieldArgument {..})
  . Map.toList $ argsMap

-- | Convert the validated field to GraphQL parser AST field
unValidateField :: G.Alias -> Field -> G.Field
unValidateField alias (Field name _ argsMap selSet) =
  let args = map (\(n, inpVal) -> G.Argument n $ unValidateInpVal inpVal) $
             Map.toList argsMap
  in G.Field (Just alias) name args [] $ unValidateSelectionSet selSet

-- | Convert the validated selection set to GraphQL parser AST selection set
unValidateSelectionSet :: SelectionSet -> G.SelectionSet
unValidateSelectionSet = \case
  SelectionSetObject selectionSet -> fromSelectionSet selectionSet
  SelectionSetInterface selectionSet -> fromScopedSelectionSet selectionSet
  SelectionSetUnion selectionSet -> fromScopedSelectionSet selectionSet
  SelectionSetNone -> mempty
  where
    fromAliasedFields :: (IsField f) => AliasedFields f -> G.SelectionSet
    fromAliasedFields =
      map (G.SelectionField . uncurry unValidateField) .
      OMap.toList . fmap toField . unAliasedFields
    fromSelectionSet =
      fromAliasedFields . unObjectSelectionSet
    toInlineSelection typeName =
      G.SelectionInlineFragment . G.InlineFragment (Just typeName) mempty .
      fromSelectionSet
    fromScopedSelectionSet (ScopedSelectionSet base specific) =
      map (uncurry toInlineSelection) (Map.toList specific) <> fromAliasedFields base

-- | Get the variable definition and it's value (if exists)
unValidateInpVariable :: AnnInpVal -> Maybe [(G.VariableDefinition,A.Value)]
unValidateInpVariable inputValue =
  case (_aivValue inputValue) of
    AGScalar _ _ -> mkVariableDefnValueTuple inputValue
    AGEnum _ _ -> mkVariableDefnValueTuple inputValue
    AGObject _ o ->
      (\obj ->
         let listObjects = OMap.toList obj
         in concat $
         mapMaybe (\(_, inpVal) -> unValidateInpVariable inpVal) listObjects)
      <$> o
    AGArray _ _ -> mkVariableDefnValueTuple inputValue
   where
     mkVariableDefnValueTuple val = maybe Nothing (\vars -> Just [vars]) $
                                     variableDefnValueTuple val

     variableDefnValueTuple :: AnnInpVal -> Maybe (G.VariableDefinition,A.Value)
     variableDefnValueTuple inpVal@AnnInpVal {..} =
      let varDefn = G.VariableDefinition <$> _aivVariable <*> Just _aivType <*> Just Nothing
      in (,) <$> varDefn <*> Just (annInpValueToJson inpVal)

-- | Convert the validated input value to GraphQL value, if the input value
-- is a variable then it will be returned without resolving it, otherwise it
-- will be resolved
unValidateInpVal :: AnnInpVal -> G.Value
unValidateInpVal (AnnInpVal _ var val) = fromMaybe G.VNull $
  -- if a variable is found, then directly return that, if not found then
  -- convert it into a G.Value and return it
  case var of
    Just var' -> Just $ G.VVariable var'
    Nothing ->
      case val of
        AGScalar _ v -> pgScalarToGValue <$> v
        AGEnum _ v -> pgEnumToGEnum v
        AGObject _ o ->
          (G.VObject . G.ObjectValueG
          . map (uncurry G.ObjectFieldG . (second unValidateInpVal))
          . OMap.toList
          ) <$> o
        AGArray _ vs -> (G.VList . G.ListValueG . map unValidateInpVal) <$> vs

  where
    pgEnumToGEnum :: AnnGEnumValue -> Maybe G.Value
    pgEnumToGEnum = \case
      AGESynthetic v -> G.VEnum <$> v
      AGEReference _ v -> (G.VEnum . G.EnumValue . G.Name . getEnumValue) <$> v

    pgScalarToGValue :: PGScalarValue -> G.Value
    pgScalarToGValue = \case
      PGValInteger i  -> G.VInt $ fromIntegral i
      PGValSmallInt i -> G.VInt $ fromIntegral i
      PGValBigInt i   -> G.VInt $ fromIntegral i
      PGValFloat f    -> G.VFloat $ realToFrac f
      PGValDouble d   -> G.VFloat $ realToFrac d
      -- TODO: Scientific is a danger zone; use its safe conv function.
      PGValNumeric sc -> G.VFloat $ realToFrac sc
      PGValMoney m    -> G.VFloat $ realToFrac m
      PGValBoolean b  -> G.VBoolean b
      PGValChar t     -> toStringValue $ T.singleton t
      PGValVarchar t  -> toStringValue t
      PGValText t     -> toStringValue t
      PGValCitext t   -> toStringValue t
      PGValDate d     -> toStringValue $ T.pack $ showGregorian d
      PGValTimeStampTZ u -> toStringValue $ T.pack $
                            formatTime defaultTimeLocale "%FT%T%QZ" u
      PGValTimeStamp u   -> toStringValue $ T.pack $
                            formatTime defaultTimeLocale "%FT%T%QZ" u
      PGValTimeTZ (ZonedTimeOfDay tod tz) ->
        toStringValue $ T.pack (show tod ++ timeZoneOffsetString tz)
      PGNull _ -> G.VNull
      PGValJSON (Q.JSON v)  -> jsonValueToGValue v
      PGValJSONB (Q.JSONB v)  -> jsonValueToGValue v
      PGValGeo v    -> jsonValueToGValue $ A.toJSON v
      PGValRaster v -> jsonValueToGValue $ A.toJSON v
      PGValUUID u    -> toStringValue $ UUID.toText u
      PGValUnknown t ->  toStringValue t
      where
        toStringValue = G.VString . G.StringValue
