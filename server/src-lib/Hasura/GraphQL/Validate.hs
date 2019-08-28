module Hasura.GraphQL.Validate
  ( validateGQ
  , showVars
  , RootSelSet(..)
  , SelSet
  , Field(..)
  , getTypedOp
  , QueryParts(..)
  , getQueryParts

  , isQueryInAllowlist

  , ReusableVariableTypes
  , ReusableVariableValues
  , validateVariablesForReuse
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Has

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as HS
import qualified Data.Sequence                          as Seq
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.RQL.Types.QueryCollection

import           Hasura.SQL.Types                       (WithScalarType)
import           Hasura.SQL.Value                       (PGScalarValue)

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

-- | Returned by 'validateGQ' when a query’s variables are sufficiently simple that this query is
-- eligible to be /reused/, which means that the resolved SQL query will not change even if any of
-- its variable values change. This is used to determine which query plans can be cached.
newtype ReusableVariableTypes = ReusableVariableTypes (Map.HashMap G.Variable PGColumnType)
  deriving (Show, Eq, ToJSON)
type ReusableVariableValues = Map.HashMap G.Variable (WithScalarType PGScalarValue)

mkReusableVariableTypes :: [G.VariableDefinition] -> AnnVarVals -> Maybe ReusableVariableTypes
mkReusableVariableTypes varDefs inputValues = ReusableVariableTypes <$> do
  -- If any of the variables are nullable, this query isn’t reusable, since a null variable used
  -- in a condition like {_eq: $var} removes the condition entirely, requiring different SQL.
  guard (not $ any (G.isNullable . G._vdType) varDefs)

  -- Note: This currently must be manually kept in sync with 'asPGColumnTypeAndValueM' from
  -- "Hasura.GraphQL.Resolve.InputValue"! It should be possible to merge them if we do #2801.
  for inputValues $ \inputValue -> case _aivValue inputValue of
    AGScalar pgType _                   -> Just $ PGColumnScalar pgType
    AGEnum _ (AGEReference reference _) -> Just $ PGColumnEnumReference reference
    _                                   -> Nothing

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
  tyInfo <- getTyInfoVE onTy
  objTyInfo <- onNothing (getObjTyM tyInfo) $ throwVE
    "fragments can only be defined on object types"
  return $ FragDef n objTyInfo selSet

data RootSelSet
  = RQuery !SelSet
  | RMutation !SelSet
  | RSubscription !Field
  deriving (Show, Eq)

-- {-# SCC validateGQ #-}
validateGQ
  :: (MonadError QErr m, MonadReader GCtx m)
  -- => GraphQLRequest
  => QueryParts
  -> m (RootSelSet, Maybe ReusableVariableTypes)
validateGQ (QueryParts opDef opRoot fragDefsL varValsM) = do

  ctx <- ask

  -- annotate the variables of this operation
  let varDefs = G._todVariableDefinitions opDef
  annVarVals <- validateVariables varDefs $ fromMaybe Map.empty varValsM
  let reusableTypes = mkReusableVariableTypes varDefs annVarVals

  -- annotate the fragments
  fragDefs <- onLeft (mkMapWith G._fdName fragDefsL) $ \dups ->
    throwVE $ "the following fragments are defined more than once: " <>
    showNames dups
  annFragDefs <- mapM validateFrag fragDefs

  -- build a validation ctx
  let valCtx = ValidationCtx (_gTypes ctx) annVarVals annFragDefs

  selSet <- flip runReaderT valCtx $ denormSelSet [] opRoot $
            G._todSelectionSet opDef

  rootSelSet <- case G._todType opDef of
    G.OperationTypeQuery -> return $ RQuery selSet
    G.OperationTypeMutation -> return $ RMutation selSet
    G.OperationTypeSubscription ->
      case Seq.viewl selSet of
        Seq.EmptyL     -> throw500 "empty selset for subscription"
        fld Seq.:< rst -> do
          unless (null rst) $
            throwVE "subscription must select only one top level field"
          return $ RSubscription fld

  pure (rootSelSet, reusableTypes)

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
