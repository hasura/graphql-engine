module Hasura.RQL.DML.Count
  ( CountQueryP1(..)
  , validateCountQWith
  , validateCountQ
  , runCount
  , countQToTx
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Builder                    as BB
import qualified Data.Sequence                              as DS

import           Control.Monad.Trans.Control                (MonadBaseControl)
import           Data.Aeson

import qualified Database.PG.Query                          as Q
import qualified Hasura.Backends.Postgres.SQL.DML           as S
import qualified Hasura.Tracing                             as Tracing

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.SQL.Types
import           Hasura.Session


data CountQueryP1
  = CountQueryP1
  { cqp1Table    :: !QualifiedTable
  , cqp1Where    :: !(AnnBoolExpSQL 'Postgres, Maybe (AnnBoolExpSQL 'Postgres))
  , cqp1Distinct :: !(Maybe [PGCol])
  } deriving (Eq)

mkSQLCount
  :: CountQueryP1 -> S.Select
mkSQLCount (CountQueryP1 tn (permFltr, mWc) mDistCols) =
  S.mkSelect
    { S.selExtr = [S.Extractor S.countStar Nothing]
    , S.selFrom = Just $ S.FromExp
                  [S.mkSelFromExp False innerSel $ TableName "r"]
    }
  where

    finalWC =
      toSQLBoolExp (S.QualTable tn) $
      maybe permFltr (andAnnBoolExps permFltr) mWc

    innerSel = partSel
      { S.selFrom  = Just $ S.mkSimpleFromExp tn
      , S.selWhere = S.WhereFrag <$> Just finalWC
      }

    partSel = case mDistCols of
      Just distCols ->
        let extrs = flip map distCols $ \c -> S.Extractor (S.mkSIdenExp c) Nothing
        in S.mkSelect
          { S.selDistinct = Just S.DistinctSimple
          , S.selExtr     = extrs
          }
      Nothing -> S.mkSelect
                 { S.selExtr = [S.Extractor (S.SEStar Nothing) Nothing] }

-- SELECT count(*) FROM (SELECT DISTINCT c1, .. cn FROM .. WHERE ..) r;
-- SELECT count(*) FROM (SELECT * FROM .. WHERE ..) r;
validateCountQWith
  :: (UserInfoM m, QErrM m, TableInfoRM 'Postgres m)
  => SessVarBldr 'Postgres m
  -> (ColumnType 'Postgres -> Value -> m S.SQLExp)
  -> CountQuery
  -> m CountQueryP1
validateCountQWith sessVarBldr prepValBldr (CountQuery qt _ mDistCols mWhere) = do
  tableInfo <- askTabInfoSource qt

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let colInfoMap = _tciFieldInfoMap $ _tiCoreInfo tableInfo

  forM_ mDistCols $ \distCols -> do
    let distColAsrns = [ checkSelOnCol selPerm
                       , assertPGCol colInfoMap relInDistColsErr]
    withPathK "distinct" $ verifyAsrns distColAsrns distCols

  -- convert the where clause
  annSQLBoolExp <- forM mWhere $ \be ->
    withPathK "where" $
    convBoolExp colInfoMap selPerm be sessVarBldr qt (valueParserWithCollectableType prepValBldr)

  resolvedSelFltr <- convAnnBoolExpPartialSQL sessVarBldr $
                     spiFilter selPerm

  return $ CountQueryP1
    qt
    (resolvedSelFltr, annSQLBoolExp)
    mDistCols
  where
    selNecessaryMsg =
      "; \"count\" is only allowed if the role "
      <> "has \"select\" permissions on the table"
    relInDistColsErr =
      "Relationships can't be used in \"distinct\"."

validateCountQ
  :: (QErrM m, UserInfoM m, CacheRM m)
  => CountQuery -> m (CountQueryP1, DS.Seq Q.PrepArg)
validateCountQ query = do
  let source = cqSource query
  tableCache :: TableCache 'Postgres <- askTableCache source
  flip runTableCacheRT (source, tableCache) $ runDMLP1T $
    validateCountQWith sessVarFromCurrentSetting binRHSBuilder query

countQToTx
  :: (QErrM m, MonadTx m)
  => (CountQueryP1, DS.Seq Q.PrepArg) -> m EncJSON
countQToTx (u, p) = do
  qRes <- liftTx $ Q.rawQE dmlTxErrorHandler
          (Q.fromBuilder countSQL) (toList p) True
  return $ encJFromBuilder $ encodeCount qRes
  where
    countSQL = toSQL $ mkSQLCount u
    encodeCount (Q.SingleRow (Identity c)) =
      BB.byteString "{\"count\":" <> BB.intDec c <> BB.char7 '}'

runCount
  :: ( QErrM m, UserInfoM m, CacheRM m
     , MonadIO m, MonadBaseControl IO m
     , Tracing.MonadTrace m, MetadataM m
     )
  => CountQuery -> m EncJSON
runCount q = do
  sourceConfig <- askSourceConfig (cqSource q)
  validateCountQ q >>= runQueryLazyTx (_pscExecCtx sourceConfig) Q.ReadOnly . countQToTx
