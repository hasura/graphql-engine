{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.GraphQL.Execute
  ( validateGQ
  , GraphQLRequest
  , runGQ
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.Aeson.Casing                 as J
import qualified Data.Aeson.TH                     as J
import qualified Data.ByteString.Lazy              as BL
import qualified Data.HashMap.Strict               as Map
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Parser     as G
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Hasura.Server.Query                as RQ

import           Hasura.GraphQL.Execute.Result
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.InputValue
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Mutation    as RM
import qualified Hasura.GraphQL.Resolve.Select      as RS

newtype GraphQLQuery
  = GraphQLQuery { unGraphQLQuery :: [G.ExecutableDefinition] }
  deriving (Show, Eq)

instance J.FromJSON GraphQLQuery where
  parseJSON = J.withText "GraphQLQuery" $ \t ->
    case G.parseExecutableDoc t of
      Left _  -> fail "parsing the graphql query failed"
      Right q -> return $ GraphQLQuery $ G.getExecutableDefinitions q

newtype OperationName
  = OperationName { _unOperationName :: G.Name }
  deriving (Show, Eq)

instance J.FromJSON OperationName where
  parseJSON v = OperationName . G.Name <$> J.parseJSON v

type VariableValues = Map.HashMap G.Variable J.Value

data GraphQLRequest
  = GraphQLRequest
  { _grOperationName :: !(Maybe OperationName)
  , _grQuery         :: !GraphQLQuery
  , _grVariables     :: !(Maybe VariableValues)
  } deriving (Show, Eq)

$(J.deriveFromJSON (J.aesonDrop 3 J.camelCase){J.omitNothingFields=True}
  ''GraphQLRequest
 )

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

{-# SCC validateGQ #-}
validateGQ
  :: (MonadError QErr m, MonadReader GCtx m)
  => GraphQLRequest
  -> m SelSet
validateGQ (GraphQLRequest opNameM q varValsM) = do

  -- get the operation that needs to be evaluated
  opDef <- getTypedOp opNameM selSets opDefs

  ctx <- ask
  -- get the operation root
  opRoot <- case G._todType opDef of
    G.OperationTypeQuery    -> return $ _gQueryRoot ctx
    G.OperationTypeMutation -> onNothing (_gMutRoot ctx) $ throwVE
                               "no mutations exist"
    _                       -> throwVE "subscriptions are not supported"

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

  flip runReaderT valCtx $ denormSelSet [] opRoot $ G._todSelectionSet opDef

  where
    (selSets, opDefs, fragDefsL) = G.partitionExDefs $ unGraphQLQuery q

{-# SCC buildTx #-}
buildTx :: UserInfo -> GCtx -> Field -> Q.TxE QErr BL.ByteString
buildTx userInfo gCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  tx <- fmap fst $ runConvert (fldMap, orderByCtx) $ case opCxt of

    OCSelect tn permFilter hdrs ->
      validateHdrs hdrs >> RS.convertSelect tn permFilter fld
    OCInsert tn vn cols hdrs    ->
      validateHdrs hdrs >> RM.convertInsert (tn, vn) cols fld
    OCUpdate tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertUpdate tn permFilter fld
    OCDelete tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertDelete tn permFilter fld
  tx
  where
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByEnums gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedHdrs = map fst $ userHeaders userInfo
      forM_ hdrs $ \hdr ->
        unless (hdr `elem` map T.toLower receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

{-# SCC resolveFld #-}
resolveFld
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtx
  -> Field -> m BL.ByteString
resolveFld pool isoL userInfo gCtx fld =
  case _fName fld of
    "__type"     -> J.encode <$> runReaderT (typeR fld) gCtx
    "__schema"   -> J.encode <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ J.encode J.Null
    _            -> runTx $ buildTx userInfo gCtx fld
  where
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> GraphQLRequest
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxMap req = do
  fields <- runReaderT (validateGQ req) gCtx
  -- putLText $ J.encodeToLazyText $ J.toJSON fields
  respFlds <- fmap V.fromList $ forM (toList fields) $ \fld -> do
    fldResp <- resolveFld pool isoL userInfo gCtx fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)
  return $ encodeGQResp $ GQSuccess $ mkJSONObj respFlds
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap
