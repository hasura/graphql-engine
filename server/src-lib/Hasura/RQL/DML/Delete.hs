module Hasura.RQL.DML.Delete
  ( validateDeleteQWith
  , validateDeleteQ
  , AnnDelG(..)
  , traverseAnnDel
  , AnnDel
  , deleteQueryToTx
  , runDelete
  ) where

import           Data.Aeson
import           Instances.TH.Lift        ()

import qualified Data.Sequence            as DS

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data AnnDelG v
  = AnnDel
  { dqp1Table   :: !QualifiedTable
  , dqp1Where   :: !(AnnBoolExp v, AnnBoolExp v)
  , dqp1MutFlds :: !(MutFldsG v)
  , dqp1AllCols :: ![PGColumnInfo]
  } deriving (Show, Eq)

traverseAnnDel
  :: (Applicative f)
  => (a -> f b)
  -> AnnDelG a
  -> f (AnnDelG b)
traverseAnnDel f annUpd =
  AnnDel tn
  <$> ((,) <$> traverseAnnBoolExp f whr <*> traverseAnnBoolExp f fltr)
  <*> traverseMutFlds f mutFlds
  <*> pure allCols
  where
    AnnDel tn (whr, fltr) mutFlds allCols = annUpd

type AnnDel = AnnDelG S.SQLExp

mkDeleteCTE
  :: AnnDel -> S.CTE
mkDeleteCTE (AnnDel tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc

validateDeleteQWith
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SessVarBldr m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> DeleteQuery
  -> m AnnDel
validateDeleteQWith sessVarBldr prepValBldr
  (DeleteQuery tableName rqlBE mRetCols) = do
  tableInfo <- askTabInfo tableName

  -- If table is view then check if it deletable
  mutableView tableName viIsDeletable
    (_tiViewInfo tableInfo) "deletable"

  -- Check if the role has delete permissions
  delPerm <- askDelPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ dpiRequiredHeaders delPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = _tiFieldInfoMap tableInfo
      allCols = getCols fieldInfoMap

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp fieldInfoMap selPerm rqlBE sessVarBldr prepValBldr

  resolvedDelFltr <- convAnnBoolExpPartialSQL sessVarBldr $
                     dpiFilter delPerm

  return $ AnnDel tableName
    (resolvedDelFltr, annSQLBoolExp)
    (mkDefaultMutFlds mAnnRetCols) allCols

  where
    selNecessaryMsg =
      "; \"delete\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateDeleteQ
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m)
  => DeleteQuery -> m (AnnDel, DS.Seq Q.PrepArg)
validateDeleteQ =
  liftDMLP1 . validateDeleteQWith sessVarFromCurrentSetting binRHSBuilder

deleteQueryToTx :: Bool -> (AnnDel, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
deleteQueryToTx strfyNum (u, p) =
  runMutation $ Mutation (dqp1Table u) (deleteCTE, p)
                (dqp1MutFlds u) (dqp1AllCols u) strfyNum
  where
    deleteCTE = mkDeleteCTE u

runDelete
  :: (QErrM m, UserInfoM m, CacheRM m, MonadTx m, HasSQLGenCtx m)
  => DeleteQuery -> m EncJSON
runDelete q = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  validateDeleteQ q >>= liftTx . deleteQueryToTx strfyNum
