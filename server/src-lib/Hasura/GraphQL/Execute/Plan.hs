{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Execute.Plan
  ( RootFieldPlan(..)
  , PGPlan(..)
  , PlanVariables
  , PrepArgMap
  , QueryPlan(..)
  , isReusable
  , mkNewQueryTx
  , mkCurPlanTx
  ) where

import           Data.Has
import           Hasura.Prelude
import           Hasura.EncJSON

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.IntMap                            as IntMap
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Resolve.InputValue
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal                (dmlTxErrorHandler)
import           Hasura.RQL.Types
import           Hasura.SQL.Value

data RootFieldPlan
  = RFPRaw !EncJSON
  | RFPPostgres !PGPlan

type PlanVariables = Map.HashMap G.Variable Int
type PrepArgMap = IntMap.IntMap Q.PrepArg

data PGPlan
  = PGPlan
  { _ppQuery     :: !Q.Query
  , _ppVariables :: !PlanVariables
  , _ppPrepared  :: !PrepArgMap
  }

data QueryPlan
  = QueryPlan
  { _qpIsSubscription :: !Bool
  , _qpVariables      :: ![G.VariableDefinition]
  , _qpFldPlans       :: ![(G.Alias, RootFieldPlan)]
  }

isReusable :: QueryPlan -> Bool
isReusable (QueryPlan _ vars fldPlans) =
  all fldPlanReusable $ map snd fldPlans
  where
    allVars = Set.fromList $ map G._vdVariable vars

    -- this is quite aggressive, we can improve this by
    -- computing used variables in each field
    allUsed fldPlanVars =
      Set.null $ Set.difference allVars $ Set.fromList fldPlanVars

    fldPlanReusable = \case
      RFPRaw _           -> True
      RFPPostgres pgPlan -> allUsed $ Map.keys $ _ppVariables pgPlan

withPlan
  :: PGPlan -> AnnVarVals -> Q.TxE QErr EncJSON
withPlan (PGPlan q reqVars prepMap) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = IntMap.elems prepMap'
  encJFromBS . runIdentity . Q.getRow <$>
    Q.rawQE dmlTxErrorHandler q args True
  where
    getVar accum (var, prepNo) = do
      let varName = G.unName $ G.unVariable var
      annVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      (_, _, _, colVal) <- asPGColVal annVal
      let prepVal = binEncoder colVal
      return $ IntMap.insert prepNo prepVal accum

-- use the existing plan and new variables to create a pg query
mkNewQueryTx
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => Maybe GH.VariableValues
  -> QueryPlan
  -> m (Bool, Q.TxE QErr EncJSON)
mkNewQueryTx varValsM (QueryPlan isSubs varDefs fldPlans) = do
  annVars <- GV.getAnnVarVals varDefs varVals
  let tx = fmap encJFromAL $ forM fldPlans $ \(alias, fldPlan) -> do
        fldResp <- case fldPlan of
          RFPRaw resp        -> return resp
          RFPPostgres pgPlan -> withPlan pgPlan annVars
        return (G.unName $ G.unAlias alias, fldResp)
  return (isSubs, tx)
  where
    varVals = fromMaybe Map.empty varValsM

-- turn the current plan into a transaction
mkCurPlanTx
  :: QueryPlan
  -> Q.TxE QErr EncJSON
mkCurPlanTx (QueryPlan _ _ fldPlans) =
  fmap encJFromAL $ forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp        -> return resp
      RFPPostgres pgPlan -> planTx pgPlan
    return (G.unName $ G.unAlias alias, fldResp)
  where
    planTx (PGPlan q _ prepMap) = do
      let args = IntMap.elems prepMap
      encJFromBS . runIdentity . Q.getRow
        <$> Q.rawQE dmlTxErrorHandler q args True
