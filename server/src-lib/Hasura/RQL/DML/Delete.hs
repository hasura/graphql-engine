module Hasura.RQL.DML.Delete
  ( validateDeleteQWith
  , validateDeleteQ
  , AnnDelG(..)
  , traverseAnnDel
  , AnnDel
  , execDeleteQuery
  , runDelete
  ) where

import           Data.Aeson
import           Instances.TH.Lift           ()

import qualified Data.Sequence               as DS

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Delete.Types
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.Server.Version       (HasVersion)
import           Hasura.RQL.Types

import qualified Database.PG.Query           as Q
import qualified Hasura.SQL.DML              as S


traverseAnnDel
  :: (Applicative f)
  => (a -> f b)
  -> AnnDelG a
  -> f (AnnDelG b)
traverseAnnDel f annUpd =
  AnnDel tn
  <$> ((,) <$> traverseAnnBoolExp f whr <*> traverseAnnBoolExp f fltr)
  <*> traverseMutationOutput f mutOutput
  <*> pure allCols
  where
    AnnDel tn (whr, fltr) mutOutput allCols = annUpd

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
  let coreInfo = _tiCoreInfo tableInfo

  -- If table is view then check if it deletable
  mutableView tableName viIsDeletable
    (_tciViewInfo coreInfo) "deletable"

  -- Check if the role has delete permissions
  delPerm <- askDelPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ dpiRequiredHeaders delPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = _tciFieldInfoMap coreInfo
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
  :: (QErrM m, UserInfoM m, CacheRM m)
  => DeleteQuery -> m (AnnDel, DS.Seq Q.PrepArg)
validateDeleteQ =
  runDMLP1T . validateDeleteQWith sessVarFromCurrentSetting binRHSBuilder

execDeleteQuery
  :: (HasVersion, MonadTx m, MonadIO m)
  => Bool
  -> Maybe MutationRemoteJoinCtx
  -> (AnnDel, DS.Seq Q.PrepArg)
  -> m EncJSON
execDeleteQuery strfyNum remoteJoinCtx (u, p) =
  runMutation $ mkMutation remoteJoinCtx (dqp1Table u) (deleteCTE, p)
                (dqp1Output u) (dqp1AllCols u) strfyNum
  where
    deleteCTE = mkDeleteCTE u

runDelete
  :: ( HasVersion, QErrM m, UserInfoM m, CacheRM m
     , MonadTx m, HasSQLGenCtx m, MonadIO m
     )
  => DeleteQuery -> m EncJSON
runDelete q = do
  strfyNum <- stringifyNum <$> askSQLGenCtx
  validateDeleteQ q >>= execDeleteQuery strfyNum Nothing
