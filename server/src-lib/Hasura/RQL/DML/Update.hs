module Hasura.RQL.DML.Update where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.HashMap.Strict      as M
import qualified Data.Sequence            as DS

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Insert    (insertCheckExpr)
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data AnnUpdG v
  = AnnUpd
  { uqp1Table   :: !QualifiedTable
  , uqp1OpExps  :: ![(PGColumnInfo, UpdOpExpG v)]
  , uqp1Where   :: !(AnnBoolExp v, AnnBoolExp v)
  , uqp1Check   :: !(AnnBoolExp v)
  -- we don't prepare the arguments for returning
  -- however the session variable can still be
  -- converted as desired
  , uqp1Output  :: !(MutationOutputG v)
  , uqp1AllCols :: ![PGColumnInfo]
  } deriving (Show, Eq)


data UpdOpExpG v = UpdSet !v
                 | UpdInc !v
                 | UpdAppend !v
                 | UpdPrepend !v
                 | UpdDeleteKey !v
                 | UpdDeleteElem !v
                 | UpdDeleteAtPath !v
                 deriving (Eq, Show, Functor, Foldable, Traversable, Generic, Data)

-- FIXME: avoid this?
updateOperatorText :: UpdOpExpG a -> Text
updateOperatorText (UpdSet          _) = "_set"
updateOperatorText (UpdInc          _) = "_inc"
updateOperatorText (UpdAppend       _) = "_append"
updateOperatorText (UpdPrepend      _) = "_prepend"
updateOperatorText (UpdDeleteKey    _) = "_delete_key"
updateOperatorText (UpdDeleteElem   _) = "_delete_elem"
updateOperatorText (UpdDeleteAtPath _) = "_delete_path_at"


{-

WIP NOTE: commented out for now, will fix / remove after deciding if
we want to keep UpdOpExpG.


traverseAnnUpd
  :: (Applicative f)
  => (a -> f b)
  -> AnnUpdG a
  -> f (AnnUpdG b)
traverseAnnUpd f annUpd =
  AnnUpd tn
  <$> traverse (traverse f) setExps
  <*> ((,) <$> traverseAnnBoolExp f whr <*> traverseAnnBoolExp f fltr)
  <*> traverseAnnBoolExp f chk
  <*> traverseMutationOutput f mutOutput
  <*> pure allCols
  where
    AnnUpd tn setExps (whr, fltr) chk mutOutput allCols = annUpd

type AnnUpd = AnnUpdG S.SQLExp

mkUpdateCTE
  :: AnnUpd -> S.CTE
mkUpdateCTE (AnnUpd tn setExps (permFltr, wc) chk _ _) =
  S.CTEUpdate update
  where
    update =
      S.SQLUpdate tn setExp Nothing tableFltr
        . Just
        . S.RetExp
        $ [ S.selectStar
          , S.Extractor (insertCheckExpr "update check constraint failed" checkExpr) Nothing
          ]
    setExp    = S.SetExp $ map S.SetExpItem setExps
    tableFltr = Just $ S.WhereFrag tableFltrExpr
    tableFltrExpr = toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps permFltr wc
    checkExpr = toSQLBoolExp (S.QualTable tn) chk

convInc
  :: (QErrM m)
  => (PGColumnType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColumnType
  -> Value
  -> m (PGCol, S.SQLExp)
convInc f col colType val = do
  prepExp <- f colType val
  return (col, S.SEOpApp S.incOp [S.mkSIdenExp col, prepExp])

convMul
  :: (QErrM m)
  => (PGColumnType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColumnType
  -> Value
  -> m (PGCol, S.SQLExp)
convMul f col colType val = do
  prepExp <- f colType val
  return (col, S.SEOpApp S.mulOp [S.mkSIdenExp col, prepExp])

convSet
  :: (QErrM m)
  => (PGColumnType -> Value -> m S.SQLExp)
  -> PGCol
  -> PGColumnType
  -> Value
  -> m (PGCol, S.SQLExp)
convSet f col colType val = do
  prepExp <- f colType val
  return (col, prepExp)

convDefault :: (Monad m) => PGCol -> PGColumnType -> () -> m (PGCol, S.SQLExp)
convDefault col _ _ = return (col, S.SEUnsafe "DEFAULT")

convOp
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap FieldInfo
  -> [PGCol]
  -> UpdPermInfo
  -> [(PGCol, a)]
  -> (PGCol -> PGColumnType -> a -> m (PGCol, S.SQLExp))
  -> m [(PGCol, S.SQLExp)]
convOp fieldInfoMap preSetCols updPerm objs conv =
  forM objs $ \(pgCol, a) -> do
    -- if column has predefined value then throw error
    when (pgCol `elem` preSetCols) $ throwNotUpdErr pgCol
    checkPermOnCol PTUpdate allowedCols pgCol
    colType <- askPGType fieldInfoMap pgCol relWhenPgErr
    res <- conv pgCol colType a
    -- build a set expression's entry
    withPathK (getPGColTxt pgCol) $ return res
  where
    allowedCols  = upiCols updPerm
    relWhenPgErr = "relationships can't be updated"
    throwNotUpdErr c = do
      role <- userRole <$> askUserInfo
      throw400 NotSupported $ "column " <> c <<> " is not updatable"
        <> " for role " <> role <<> "; its value is predefined in permission"

validateUpdateQueryWith
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SessVarBldr m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> UpdateQuery
  -> m AnnUpd
validateUpdateQueryWith sessVarBldr prepValBldr uq = do
  let tableName = uqTable uq
  tableInfo <- withPathK "table" $ askTabInfo tableName
  let coreInfo = _tiCoreInfo tableInfo

  -- If it is view then check if it is updatable
  mutableView tableName viIsUpdatable
    (_tciViewInfo coreInfo) "updatable"

  -- Check if the role has update permissions
  updPerm <- askUpdPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ upiRequiredHeaders updPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = _tciFieldInfoMap coreInfo
      allCols = getCols fieldInfoMap
      preSetObj = upiSet updPerm
      preSetCols = M.keys preSetObj

  -- convert the object to SQL set expression
  setItems <- withPathK "$set" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqSet uq) $ convSet prepValBldr

  incItems <- withPathK "$inc" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqInc uq) $ convInc prepValBldr

  mulItems <- withPathK "$mul" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqMul uq) $ convMul prepValBldr

  defItems <- withPathK "$default" $
    convOp fieldInfoMap preSetCols updPerm (zip (uqDefault uq) [()..]) convDefault

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  resolvedPreSetItems <- M.toList <$>
                         mapM (convPartialSQLExp sessVarBldr) preSetObj

  let setExpItems = resolvedPreSetItems ++ setItems ++ incItems ++
                    mulItems ++ defItems

  when (null setExpItems) $
    throw400 UnexpectedPayload "atleast one of $set, $inc, $mul has to be present"

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp fieldInfoMap selPerm (uqWhere uq) sessVarBldr prepValBldr

  resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr $
                     upiFilter updPerm
  resolvedUpdCheck <- fromMaybe gBoolExpTrue <$>
                        traverse (convAnnBoolExpPartialSQL sessVarBldr)
                          (upiCheck updPerm)

  return $ AnnUpd
    tableName
    setExpItems
    (resolvedUpdFltr, annSQLBoolExp)
    resolvedUpdCheck
    (mkDefaultMutFlds mAnnRetCols)
    allCols
  where
    mRetCols = uqReturning uq
    selNecessaryMsg =
      "; \"update\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateUpdateQuery
  :: (QErrM m, UserInfoM m, CacheRM m)
  => UpdateQuery -> m (AnnUpd, DS.Seq Q.PrepArg)
validateUpdateQuery =
  runDMLP1T . validateUpdateQueryWith sessVarFromCurrentSetting binRHSBuilder

updateQueryToTx
  :: Bool -> (AnnUpd, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
updateQueryToTx strfyNum (u, p) =
  runMutation $ Mutation (uqp1Table u) (updateCTE, p)
                (uqp1Output u) (uqp1AllCols u) strfyNum
  where
    updateCTE = mkUpdateCTE u

runUpdate
  :: (QErrM m, UserInfoM m, CacheRM m, MonadTx m, HasSQLGenCtx m)
  => UpdateQuery -> m EncJSON
runUpdate q = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  validateUpdateQuery q >>= liftTx . updateQueryToTx strfyNum

-}
