module Hasura.RQL.DML.Update
  ( validateUpdateQueryWith
  , validateUpdateQuery
  , UpdateQueryP1(..)
  , updateQueryToTx
  , getUpdateDeps
  , runUpdate
  ) where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.HashMap.Strict      as M
import qualified Data.Sequence            as DS

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data UpdateQueryP1
  = UpdateQueryP1
  { uqp1Table   :: !QualifiedTable
  , uqp1SetExps :: ![(PGCol, S.SQLExp)]
  , uqp1Where   :: !(AnnBoolExpSQL, AnnBoolExpSQL)
  , pqp1MutFlds :: !MutFlds
  } deriving (Show, Eq)

mkSQLUpdate
  :: UpdateQueryP1 -> S.SelectWith
mkSQLUpdate (UpdateQueryP1 tn setExps (permFltr, wc) mutFlds) =
  mkSelWith tn (S.CTEUpdate update) mutFlds False
  where
    update = S.SQLUpdate tn setExp Nothing tableFltr $ Just S.returningStar
    setExp    = S.SetExp $ map S.SetExpItem setExps
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps permFltr wc

getUpdateDeps
  :: UpdateQueryP1
  -> [SchemaDependency]
getUpdateDeps (UpdateQueryP1 tn setExps (_, wc) mutFlds) =
  mkParentDep tn : colDeps <> whereDeps <> retDeps
  where
    colDeps   = map (mkColDep "on_type" tn . fst) setExps
    whereDeps = getBoolExpDeps tn wc
    retDeps   = map (mkColDep "untyped" tn . fst) $
                pgColsFromMutFlds mutFlds

convInc
  :: (QErrM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColType
  -> Value
  -> m (PGCol, S.SQLExp)
convInc f col colType val = do
  prepExp <- f colType val
  return (col, S.SEOpApp S.incOp [S.mkSIdenExp col, prepExp])

convMul
  :: (QErrM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColType
  -> Value
  -> m (PGCol, S.SQLExp)
convMul f col colType val = do
  prepExp <- f colType val
  return (col, S.SEOpApp S.mulOp [S.mkSIdenExp col, prepExp])

convSet
  :: (QErrM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColType
  -> Value
  -> m (PGCol, S.SQLExp)
convSet f col colType val = do
  prepExp <- f colType val
  return (col, prepExp)

convDefault :: (Monad m) => PGCol -> PGColType -> () -> m (PGCol, S.SQLExp)
convDefault col _ _ = return (col, S.SEUnsafe "DEFAULT")

convOp
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap
  -> UpdPermInfo
  -> [(PGCol, a)]
  -> (PGCol -> PGColType -> a -> m (PGCol, S.SQLExp))
  -> m [(PGCol, S.SQLExp)]
convOp fieldInfoMap updPerm objs conv =
  forM objs $ \(pgCol, a) -> do
    checkPermOnCol PTUpdate allowedCols pgCol
    colType <- askPGType fieldInfoMap pgCol relWhenPgErr
    res <- conv pgCol colType a
    -- build a set expression's entry
    withPathK (getPGColTxt pgCol) $ return res
  where
    allowedCols  = upiCols updPerm
    relWhenPgErr = "relationships can't be updated"

validateUpdateQueryWith
  :: (UserInfoM m, QErrM m, CacheRM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> UpdateQuery
  -> m UpdateQueryP1
validateUpdateQueryWith f uq = do
  let tableName = uqTable uq
  tableInfo <- withPathK "table" $ askTabInfo tableName

  -- If it is view then check if it is updatable
  mutableView tableName viIsUpdatable
    (tiViewInfo tableInfo) "updatable"

  -- Check if the role has update permissions
  updPerm <- askUpdPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ upiRequiredHeaders updPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = tiFieldInfoMap tableInfo

  -- convert the object to SQL set expression
  setItems <- withPathK "$set" $
    convOp fieldInfoMap updPerm (M.toList $ uqSet uq) $ convSet f

  incItems <- withPathK "$inc" $
    convOp fieldInfoMap updPerm (M.toList $ uqInc uq) $ convInc f

  mulItems <- withPathK "$mul" $
    convOp fieldInfoMap updPerm (M.toList $ uqMul uq) $ convMul f

  defItems <- withPathK "$default" $
    convOp fieldInfoMap updPerm (zip (uqDefault uq) [()..]) convDefault

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  let setExpItems = setItems ++ incItems ++ mulItems ++ defItems

  when (null setExpItems) $
    throw400 UnexpectedPayload "atleast one of $set, $inc, $mul has to be present"

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp' fieldInfoMap selPerm (uqWhere uq) f

  return $ UpdateQueryP1
    tableName
    setExpItems
    (upiFilter updPerm, annSQLBoolExp)
    (mkDefaultMutFlds mAnnRetCols)
  where
    mRetCols = uqReturning uq
    selNecessaryMsg =
      "; \"update\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateUpdateQuery
  :: (QErrM m, UserInfoM m, CacheRM m)
  => UpdateQuery -> m (UpdateQueryP1, DS.Seq Q.PrepArg)
validateUpdateQuery =
  liftDMLP1 . validateUpdateQueryWith binRHSBuilder

updateQueryToTx :: (UpdateQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
updateQueryToTx (u, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder updateSQL) (toList p) True
  where
    updateSQL = toSQL $ mkSQLUpdate u

runUpdate
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => UpdateQuery -> m RespBody
runUpdate q =
  validateUpdateQuery q >>= liftTx . updateQueryToTx
