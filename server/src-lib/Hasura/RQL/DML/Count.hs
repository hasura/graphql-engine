module Hasura.RQL.DML.Count
  ( CountQueryP1 (..),
    validateCountQWith,
    validateCountQ,
    runCount,
    countQToTx,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.ByteString.Builder qualified as BB
import Data.Sequence qualified as DS
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModel.Cache (LogicalModelCache, LogicalModelInfo (..))
import Hasura.LogicalModel.Fields (LogicalModelFieldsRM, runLogicalModelFieldsLookup)
import Hasura.Prelude
import Hasura.RQL.DML.Internal
import Hasura.RQL.DML.Types
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types
import Hasura.Session
import Hasura.Table.Cache
import Hasura.Tracing qualified as Tracing

data CountQueryP1 = CountQueryP1
  { cqp1Table :: QualifiedTable,
    cqp1Where :: (AnnBoolExpSQL ('Postgres 'Vanilla), Maybe (AnnBoolExpSQL ('Postgres 'Vanilla))),
    cqp1Distinct :: Maybe [PGCol]
  }
  deriving (Eq)

mkSQLCount ::
  (MonadIO m, MonadError QErr m) =>
  UserInfo ->
  CountQueryP1 ->
  m S.Select
mkSQLCount userInfo (CountQueryP1 tn (permFltr, mWc) mDistCols) = do
  finalWC <-
    toSQLBoolExp userInfo (S.QualTable tn)
      $ maybe permFltr (andAnnBoolExps permFltr) mWc
  let innerSel =
        partSel
          { S.selFrom = Just $ S.mkSimpleFromExp tn,
            S.selWhere = S.WhereFrag <$> Just finalWC
          }
  pure
    $ S.mkSelect
      { S.selExtr = [S.Extractor S.countStar Nothing],
        S.selFrom =
          Just
            $ S.FromExp
              [S.mkSelFromExp False innerSel $ TableName "r"]
      }
  where
    partSel = case mDistCols of
      Just distCols ->
        let extrs = flip map distCols $ \c -> S.Extractor (S.mkSIdenExp c) Nothing
         in S.mkSelect
              { S.selDistinct = Just S.DistinctSimple,
                S.selExtr = extrs
              }
      Nothing ->
        S.mkSelect
          { S.selExtr = [S.Extractor (S.SEStar Nothing) Nothing]
          }

-- SELECT count(*) FROM (SELECT DISTINCT c1, .. cn FROM .. WHERE ..) r;
-- SELECT count(*) FROM (SELECT * FROM .. WHERE ..) r;
validateCountQWith ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m, LogicalModelFieldsRM ('Postgres 'Vanilla) m) =>
  SessionVariableBuilder m ->
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  CountQuery ->
  m CountQueryP1
validateCountQWith sessVarBldr prepValBldr (CountQuery qt _ mDistCols mWhere) = do
  tableInfo <- askTableInfoSource qt

  -- Check if select is allowed
  selPerm <-
    modifyErr (<> selNecessaryMsg)
      $ askSelPermInfo tableInfo

  let colInfoMap = _tciFieldInfoMap $ _tiCoreInfo tableInfo

  forM_ mDistCols $ \distCols -> do
    let distColAsrns =
          [ checkSelOnCol selPerm,
            assertColumnExists colInfoMap relInDistColsErr
          ]
    withPathK "distinct" $ verifyAsrns distColAsrns distCols

  -- convert the where clause
  annSQLBoolExp <- forM mWhere $ \be ->
    withPathK "where"
      $ convBoolExp colInfoMap selPerm be sessVarBldr colInfoMap (valueParserWithCollectableType prepValBldr)

  resolvedSelFltr <-
    convAnnBoolExpPartialSQL sessVarBldr
      $ spiFilter selPerm

  return
    $ CountQueryP1
      qt
      (resolvedSelFltr, annSQLBoolExp)
      mDistCols
  where
    selNecessaryMsg =
      "; \"count\" is only allowed if the role "
        <> "has \"select\" permissions on the table"
    relInDistColsErr =
      "Relationships can't be used in \"distinct\"."

validateCountQ ::
  (QErrM m, UserInfoM m, CacheRM m) =>
  CountQuery ->
  m (CountQueryP1, DS.Seq PG.PrepArg)
validateCountQ query = do
  let source = cqSource query
  tableCache :: TableCache ('Postgres 'Vanilla) <- fold <$> askTableCache source
  logicalModelCache :: LogicalModelCache ('Postgres 'Vanilla) <- fold <$> askLogicalModelCache source
  flip runTableCacheRT tableCache
    $ runLogicalModelFieldsLookup _lmiFields logicalModelCache
    $ runDMLP1T
    $ validateCountQWith sessVarFromCurrentSetting binRHSBuilder query

countQToTx ::
  (MonadTx m, MonadIO m) =>
  UserInfo ->
  (CountQueryP1, DS.Seq PG.PrepArg) ->
  m EncJSON
countQToTx userInfo (u, p) = do
  countSQL <- toSQL <$> mkSQLCount userInfo u
  qRes <-
    liftTx
      $ PG.rawQE
        dmlTxErrorHandler
        (PG.fromBuilder countSQL)
        (toList p)
        True
  return $ encJFromBuilder $ encodeCount qRes
  where
    encodeCount (PG.SingleRow (Identity c)) =
      BB.byteString "{\"count\":" <> BB.intDec c <> BB.char7 '}'

-- TODO: What does this do?
runCount ::
  ( QErrM m,
    UserInfoM m,
    CacheRM m,
    MonadIO m,
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    MetadataM m
  ) =>
  CountQuery ->
  m EncJSON
runCount q = do
  userInfo <- askUserInfo
  sourceConfig <- askSourceConfig @('Postgres 'Vanilla) (cqSource q)
  validateCountQ q >>= runTxWithCtx (_pscExecCtx sourceConfig) (Tx PG.ReadOnly Nothing) LegacyRQLQuery . countQToTx userInfo
