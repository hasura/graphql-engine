module Hasura.RQL.DML.Delete
  ( validateDeleteQWith
  , validateDeleteQ
  , DeleteQueryP1(..)
  , deleteQueryToTx
  , getDeleteDeps
  , runDelete
  ) where

import           Data.Aeson
import           Instances.TH.Lift        ()

import qualified Data.Sequence            as DS

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data DeleteQueryP1
  = DeleteQueryP1
  { dqp1Table   :: !QualifiedTable
  , dqp1Where   :: !(AnnBoolExpSQL, AnnBoolExpSQL)
  , dqp1MutFlds :: !MutFlds
  } deriving (Show, Eq)

mkSQLDelete
  :: DeleteQueryP1 -> S.SelectWith
mkSQLDelete (DeleteQueryP1 tn (fltr, wc) mutFlds) =
  mkSelWith tn (S.CTEDelete delete) mutFlds False
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc

getDeleteDeps
  :: DeleteQueryP1 -> [SchemaDependency]
getDeleteDeps (DeleteQueryP1 tn (_, wc) mutFlds) =
  mkParentDep tn : whereDeps <> retDeps
  where
    whereDeps = getBoolExpDeps tn wc
    retDeps   = map (mkColDep "untyped" tn . fst) $
                pgColsFromMutFlds mutFlds

validateDeleteQWith
  :: (UserInfoM m, QErrM m, CacheRM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> DeleteQuery
  -> m DeleteQueryP1
validateDeleteQWith prepValBuilder (DeleteQuery tableName rqlBE mRetCols) = do
  tableInfo <- askTabInfo tableName

  -- If table is view then check if it deletable
  mutableView tableName viIsDeletable
    (tiViewInfo tableInfo) "deletable"

  -- Check if the role has delete permissions
  delPerm <- askDelPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ dpiRequiredHeaders delPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = tiFieldInfoMap tableInfo

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp' fieldInfoMap selPerm rqlBE prepValBuilder

  return $ DeleteQueryP1 tableName
    (dpiFilter delPerm, annSQLBoolExp)
    (mkDefaultMutFlds mAnnRetCols)

  where
    selNecessaryMsg =
      "; \"delete\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateDeleteQ
  :: (QErrM m, UserInfoM m, CacheRM m)
  => DeleteQuery -> m (DeleteQueryP1, DS.Seq Q.PrepArg)
validateDeleteQ =
  liftDMLP1 . validateDeleteQWith binRHSBuilder

deleteQueryToTx :: (DeleteQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
deleteQueryToTx (u, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder deleteSQL) (toList p) True
  where
    deleteSQL = toSQL $ mkSQLDelete u

runDelete
  :: (QErrM m, UserInfoM m, CacheRM m, MonadTx m)
  => DeleteQuery -> m RespBody
runDelete q =
  validateDeleteQ q >>= liftTx . deleteQueryToTx
