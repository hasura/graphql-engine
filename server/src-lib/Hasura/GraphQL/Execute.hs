{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Hasura.GraphQL.Execute
  ( EC.QueryCache
  , EC.initQueryCache
  , EC.clearQueryCache

  , EP.QueryPlan(..)
  , EP.PGPlan(..)
  , EP.RootFieldPlan

  , GQExecPlan(..)
  , getGQExecPlan

  -- TODO: should these be exposed?
  , getTopLevelNodes
  , gatherTypeLocs
  , assertSameLocationNodes
  ) where

import           Data.Word                              (Word64)
import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.QueryCache      as EC
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT

getHQueryPlan
  :: (MonadIO m, MonadError QErr m)
  => UserInfo
  -> Word64 -- schema version
  -> GCtx
  -> EC.QueryCache
  -> GQLReqParsed
  -> VQ.QueryParts
  -> m (G.OperationType, Q.TxE QErr EncJSON)
getHQueryPlan userInfo schemaVer gCtx queryCache req queryParts = do

  queryPlanM <- liftIO $ EC.getPlan schemaVer (userRole userInfo)
                req queryCache
  case queryPlanM of
    Just queryPlan -> do
      (isSubs, tx) <- flip runReaderT gCtx $
        EP.mkNewQueryTx (_grVariables req) queryPlan
      let opTy = bool G.OperationTypeQuery G.OperationTypeSubscription isSubs
      return (opTy, tx)

    Nothing -> do
      -- validate the query
      (varDefs, opTy, fields) <- runReaderT (VQ.validateGQ queryParts) gCtx
      -- create the transaction
      (opTy,) <$> case opTy of
        G.OperationTypeMutation ->
          return $ R.resolveMutationSelSet userInfo gCtx fields
        _ -> do
          let isSubs = opTy == G.OperationTypeSubscription
          queryPlan <- EP.QueryPlan isSubs varDefs <$>
                       R.resolveQuerySelSet userInfo gCtx fields
          when (EP.isReusable queryPlan) $
            liftIO $ EC.addPlan schemaVer (userRole userInfo) req
            queryPlan queryCache
          return $ EP.mkCurPlanTx queryPlan

data GQExecPlan
  = GExPHasura !G.OperationType !(Q.TxE QErr EncJSON)
  | GExPRemote !RemoteSchemaInfo !G.TypedOperationDefinition

getGQExecPlan
  :: (MonadIO m, MonadError QErr m)
  => EC.QueryCache
  -> UserInfo
  -> Word64 -- schema version
  -> GCtx
  -> GQLReqUnparsed
  -> m (GQLReqParsed, GQExecPlan)
getGQExecPlan queryCache userInfo schemaVer gCtx unParsedReq = do

  -- look in the ast cache
  astM <- liftIO $ EC.getAST (_grQuery unParsedReq) queryCache

  req  <- case astM of
    -- construct a parsed request with the cached ast
    Just ast ->
      return $ unParsedReq { _grQuery = ast}
    -- parse and add it to the cache
    Nothing  -> do
      req <- toParsed unParsedReq
      liftIO $ EC.addAST (_grQuery unParsedReq) (_grQuery req) queryCache
      return req

  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- see if they are all the same
  typeLoc <- assertSameLocationNodes typeLocs

  (req,) <$> case typeLoc of

    VT.RemoteType _ rsi ->
      return $ GExPRemote rsi opDef

    VT.HasuraType -> do
      (opTy, tx) <- getHQueryPlan userInfo schemaVer
                    gCtx queryCache req queryParts
      return $ GExPHasura opTy tx

assertSameLocationNodes
  :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
assertSameLocationNodes typeLocs =
  case Set.toList (Set.fromList typeLocs) of
    -- this shouldn't happen
    []    -> return VT.HasuraType
    [loc] -> return loc
    _     -> throw400 NotSupported msg
  where
    msg = "cannot mix top level fields from two different graphql servers"

-- TODO: we should retire the function asap
getTopLevelNodes :: G.TypedOperationDefinition -> [G.Name]
getTopLevelNodes opDef =
  mapMaybe f $ G._todSelectionSet opDef
  where
    -- TODO: this will fail when there is a fragment at the top level
    f = \case
      G.SelectionField fld        -> Just $ G._fName fld
      G.SelectionFragmentSpread _ -> Nothing
      G.SelectionInlineFragment _ -> Nothing

gatherTypeLocs :: GCtx -> [G.Name] -> [VT.TypeLoc]
gatherTypeLocs gCtx nodes =
  catMaybes $ flip map nodes $ \node ->
    VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe qr (Map.union qr) mr
