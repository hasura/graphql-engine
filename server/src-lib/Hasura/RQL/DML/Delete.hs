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

data DeleteQueryP1
  = DeleteQueryP1
  { dqp1Table    :: !QualifiedTable
  , dqp1Where    :: !(AnnBoolExpSQL, AnnBoolExpSQL)
  , dqp1MutFlds  :: !MutFlds
  , dqp1UniqCols :: !(Maybe [PGColInfo])
  } deriving (Show, Eq)

mkDeleteCTE
  :: DeleteQueryP1 -> S.CTE
mkDeleteCTE (DeleteQueryP1 tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc

getDeleteDeps
  :: DeleteQueryP1 -> [SchemaDependency]
getDeleteDeps (DeleteQueryP1 tn (_, wc) mutFlds uniqCols) =
  mkParentDep tn : uniqColDeps <> whereDeps <> retDeps
  where
    whereDeps = getBoolExpDeps tn wc
    uniqColDeps = map (mkColDep "on_type" tn) $
                  maybe [] (map pgiName) uniqCols
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
      uniqCols = getUniqCols (getCols fieldInfoMap) $
                 tiUniqOrPrimConstraints tableInfo

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp' fieldInfoMap selPerm rqlBE prepValBuilder

  return $ DeleteQueryP1 tableName
    (dpiFilter delPerm, annSQLBoolExp)
    (mkDefaultMutFlds mAnnRetCols) uniqCols

  where
    selNecessaryMsg =
      "; \"delete\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateDeleteQ
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m)
  => DeleteQuery -> m (DeleteQueryP1, DS.Seq Q.PrepArg)
validateDeleteQ =
  liftDMLP1 . validateDeleteQWith binRHSBuilder

deleteQueryToTx :: Bool -> (DeleteQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
deleteQueryToTx strfyNum (u, p) =
  runMutation $ Mutation (dqp1Table u) (deleteCTE, p)
                (dqp1MutFlds u) (dqp1UniqCols u) strfyNum
  where
    deleteCTE = mkDeleteCTE u

runDelete
  :: (QErrM m, UserInfoM m, CacheRM m, MonadTx m, HasSQLGenCtx m)
  => DeleteQuery -> m EncJSON
runDelete q = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  validateDeleteQ q >>= liftTx . deleteQueryToTx strfyNum
