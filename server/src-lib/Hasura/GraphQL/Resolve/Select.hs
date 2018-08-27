{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.Select
  ( convertSelect2
  , runPlanM
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.IntMap                       as IntMap
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Select             as RS

import qualified Hasura.SQL.DML                    as S

import qualified Hasura.GraphQL.Execute.Plan       as Plan
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (onlyPositiveInt)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data PlanningSt
  = PlanningSt
  { _psArgNumber :: !Int
  , _psVariables :: !Plan.PlanVariables
  , _psPrepped   :: !Plan.PrepArgMap
  }

initPlanningSt :: PlanningSt
initPlanningSt = PlanningSt 1 Map.empty IntMap.empty

type PlanM =
  StateT PlanningSt (ReaderT (FieldMap, OrdByResolveCtx) (Except QErr))

runPlanM
  :: (MonadError QErr m)
  => (FieldMap, OrdByResolveCtx) -> PlanM Q.Query -> m Plan.RootFieldPlan
runPlanM ctx m = do
  (q, PlanningSt _ vars prepped) <- either throwError return $
    runExcept $ runReaderT (runStateT m initPlanningSt) ctx
  return $ Plan.RFPPostgres $ Plan.PGPlan q vars prepped

getVarArgNum
  :: (MonadState PlanningSt m)
  => G.Variable -> m Int
getVarArgNum var = do
  PlanningSt curArgNum vars prepped <- get
  case Map.lookup var vars of
    Just argNum -> return argNum
    Nothing     -> do
      put $ PlanningSt (curArgNum + 1) (Map.insert var curArgNum vars) prepped
      return curArgNum

addPrepArg
  :: (MonadState PlanningSt m)
  => Int -> Q.PrepArg -> m ()
addPrepArg argNum arg = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt curArgNum vars $ IntMap.insert argNum arg prepped

getNextArgNum
  :: (MonadState PlanningSt m)
  => m Int
getNextArgNum = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt (curArgNum + 1) vars prepped
  return curArgNum

prepare2
  :: (MonadState PlanningSt m)
  => AnnPGVal -> m S.SQLExp
prepare2 (varM, isNullable, colTy, colVal) = do
  argNum <- case (varM, isNullable) of
    (Just var, False) -> getVarArgNum var
    _                 -> getNextArgNum
  addPrepArg argNum $ binEncoder colVal
  return $ toPrepParam argNum colTy

fromSelSet
  :: G.NamedType
  -> SelSet
  -> PlanM (Map.HashMap FieldName RS.AnnFld)
fromSelSet fldTy flds =
  fmap Map.fromList $ forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    case fldName of
      "__typename" -> return (rqlFldName, RS.FExp $ G.unName $ G.unNamedType fldTy)
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          Left (PGColInfo pgCol colTy _) -> return (rqlFldName, RS.FCol (pgCol, colTy))
          Right (relInfo, tableFilter, tableLimit, _) -> do
            let relTN = riRTable relInfo
            relSelData <- fromField relTN tableFilter tableLimit fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) relSelData
            return (rqlFldName, RS.FRel annRel)

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath fld = nameAsPath $ _fName fld

fromField
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> PlanM RS.SelectData
fromField tn permFilter permLimit fld = fieldAsPath fld $ do
  whereExpM  <- withArgM args "where" $ convertBoolExpG prepare2 tn
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- RS.applyPermLimit permLimit
                <$> withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> prepare2
  annFlds    <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.SelectData annFlds tn (permFilter, whereExpM) ordByExpM
    [] limitExpM offsetExpM
  where
    args = _fArguments fld

getEnumInfo
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByResolveCtx r
     )
  => G.NamedType -> G.EnumValue -> m OrdByResolveCtxElem
getEnumInfo nt v = do
  -- fldMap <- _gcFieldMap <$> ask
  ordByCtx <- asks getter
  onNothing (Map.lookup (nt,v) ordByCtx) $
    throw500 $ "could not lookup " <> showName (G.unEnumValue v) <> " in " <>
    showNamedTy nt

parseOrderBy
  :: (MonadError QErr m
     , MonadReader r m
     , Has OrdByResolveCtx r
     )
  => AnnInpVal -> m S.OrderByExp
parseOrderBy v = do
  enums <- withArray (const $ mapM asEnumVal) v
  fmap S.OrderByExp $ forM enums $ \(nt, ev) ->
    convOrdByElem <$> getEnumInfo nt ev
  -- return $ map convOrdByElem enums
  -- undefined
  where
    convOrdByElem (PGColInfo col _ _, ordTy, nullsOrd) =
      S.OrderByItem (Left col)
      (Just $ convOrdTy ordTy)
      (Just $ convNullsOrd nullsOrd)

    convOrdTy = \case
      OAsc  -> S.OTAsc
      ODesc -> S.OTDesc

    convNullsOrd = \case
      NFirst -> S.NFirst
      NLast  -> S.NLast

parseLimit :: ( MonadError QErr m ) => AnnInpVal -> m Int
parseLimit v = do
  (_, _, _, pgColVal) <- asPGColVal v
  limit <- maybe noIntErr return $ pgColValueToInt pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throwVE "expecting Integer value for \"limit\""

-- convertSelect
--   :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> PlanM RespTx
-- convertSelect qt permFilter permLimit fld = do
--   selData <- withPathK "selectionSet" $
--              fromField qt permFilter permLimit fld
--   prepArgs <- get
--   return $ RS.selectP2 (selData, prepArgs)

convertSelect2
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field
  -> PlanM Q.Query
convertSelect2 qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromField qt permFilter permLimit fld
  return $ Q.fromBuilder $ toSQL $ RS.mkSQLSelect selData
