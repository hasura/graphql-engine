module Hasura.GraphQL.Validate
  ( validateGQ
  , getTypedOp
  , GraphQLRequest
  , QueryParts (..)
  , getQueryParts
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types


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

-- For all the variables defined there will be a value in the final map
-- If no default, not in variables and nullable, then null value
getAnnVarVals
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m
     )
  => [G.VariableDefinition]
  -> VariableValues
  -> m AnnVarVals
getAnnVarVals varDefsL inpVals = do

  varDefs <- onLeft (mkMapWith G._vdVariable varDefsL) $ \dups ->
    throwVE $ "the following variables are defined more than once: " <>
    showVars dups

  let unexpectedVars = filter (not . (`Map.member` varDefs)) $ Map.keys inpVals

  unless (null unexpectedVars) $
    throwVE $ "unexpected variables in variableValues: " <>
    showVars unexpectedVars

  forM varDefs $ \(G.VariableDefinition var ty defM) -> do
    let baseTy = getBaseTy ty
    baseTyInfo <- getTyInfoVE baseTy
    -- check that the variable is defined on input types
    when (isObjTy baseTyInfo) $ throwVE $ objTyErrMsg baseTy

    let defM' = bool (defM <|> Just G.VCNull) defM $ G.isNotNull ty
    annDefM <- withPathK "defaultValue" $
               mapM (validateInputValue constValueParser ty) defM'
    let inpValM = Map.lookup var inpVals
    annInpValM <- withPathK "variableValues" $
                  mapM (validateInputValue jsonParser ty) inpValM
    let varValM = annInpValM <|> annDefM
    onNothing varValM $ throwVE $ "expecting a value for non-null type: "
      <> G.showGT ty <> " in variableValues"
  where
    objTyErrMsg namedTy =
      "variables can only be defined on input types"
      <> "(enums, scalars, input objects), but "
      <> showNamedTy namedTy <> " is an object type"

    showVars :: (Functor f, Foldable f) => f G.Variable -> Text
    showVars = showNames . fmap G.unVariable

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

-- {-# SCC validateGQ #-}
validateGQ
  :: (MonadError QErr m, MonadReader GCtx m)
  -- => GraphQLRequest
  => QueryParts
  -> m (G.OperationType, SelSet)
validateGQ (QueryParts opDef opRoot fragDefsL varValsM) = do

  -- get the operation that needs to be evaluated
  --opDef <- getTypedOp opNameM selSets opDefs

  ctx <- ask
  -- -- get the operation root
  -- opRoot <- case G._todType opDef of
  --   G.OperationTypeQuery        -> return $ _gQueryRoot ctx
  --   G.OperationTypeMutation     ->
  --     onNothing (_gMutRoot ctx) $ throwVE "no mutations exist"
  --   G.OperationTypeSubscription ->
  --     onNothing (_gSubRoot ctx) $ throwVE "no subscriptions exist"

  -- annotate the variables of this operation
  annVarVals <- getAnnVarVals (G._todVariableDefinitions opDef) $
                fromMaybe Map.empty varValsM

  -- annotate the fragments
  fragDefs <- onLeft (mkMapWith G._fdName fragDefsL) $ \dups ->
    throwVE $ "the following fragments are defined more than once: " <>
    showNames dups
  annFragDefs <- mapM validateFrag fragDefs

  -- build a validation ctx
  let valCtx = ValidationCtx (_gTypes ctx) annVarVals annFragDefs

  selSet <- flip runReaderT valCtx $ denormSelSet [] opRoot $
            G._todSelectionSet opDef

  when (G._todType opDef == G.OperationTypeSubscription && length selSet > 1) $
    throwVE "subscription must select only one top level field"

  return (G._todType opDef, selSet)


getQueryParts
  :: ( MonadError QErr m, MonadReader GCtx m)
  => GraphQLRequest
  -> m QueryParts
getQueryParts (GraphQLRequest opNameM q varValsM) = do
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
    (selSets, opDefs, fragDefsL) = G.partitionExDefs $ unGraphQLQuery q
