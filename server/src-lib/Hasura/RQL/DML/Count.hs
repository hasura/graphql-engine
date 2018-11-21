{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.Count where

import           Data.Aeson
import           Instances.TH.Lift       ()

import qualified Data.ByteString.Builder as BB
import qualified Data.Sequence           as DS

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query       as Q
import qualified Hasura.SQL.DML          as S

data CountQueryP1
  = CountQueryP1
  { cqp1Table    :: !QualifiedTable
  , cqp1Where    :: !(S.BoolExp, Maybe (GBoolExp AnnSQLBoolExp))
  , cqp1Distinct :: !(Maybe [PGCol])
  } deriving (Show, Eq)

getCountDeps
  :: CountQueryP1 -> [SchemaDependency]
getCountDeps (CountQueryP1 tn (_, mWc) mDistCols) =
  mkParentDep tn
  : fromMaybe [] whereDeps
  <> fromMaybe [] distDeps
  where
    distDeps   = map (mkColDep "untyped" tn) <$> mDistCols
    whereDeps   = getBoolExpDeps tn <$> mWc

mkSQLCount
  :: CountQueryP1 -> S.Select
mkSQLCount (CountQueryP1 tn (permFltr, mWc) mDistCols) =
  S.mkSelect
    { S.selExtr = [S.Extractor (S.SEFnApp "count" [S.SEStar] Nothing) Nothing]
    , S.selFrom = Just $ S.FromExp
                  [S.mkSelFromExp False innerSel $ TableName "r"]
    }
  where

    finalWC =
      S.BEBin S.AndOp permFltr $
      maybe (S.BELit True) cBoolExp mWc

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
                 { S.selExtr = [S.Extractor S.SEStar Nothing] }

-- SELECT count(*) FROM (SELECT DISTINCT c1, .. cn FROM .. WHERE ..) r;
-- SELECT count(*) FROM (SELECT * FROM .. WHERE ..) r;
countP1
  :: (P1C m)
  => (PGColType -> Value -> m S.SQLExp)
  -> CountQuery
  -> m CountQueryP1
countP1 prepValBuilder (CountQuery qt mDistCols mWhere) = do
  tableInfo <- askTabInfo qt

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let colInfoMap = tiFieldInfoMap tableInfo

  forM_ mDistCols $ \distCols -> do
    let distColAsrns = [ checkSelOnCol selPerm
                       , assertPGCol colInfoMap relInDistColsErr]
    withPathK "distinct" $ verifyAsrns distColAsrns distCols

  -- convert the where clause
  annSQLBoolExp <- forM mWhere $ \be ->
    withPathK "where" $
    convBoolExp' colInfoMap qt selPerm be prepValBuilder

  return $ CountQueryP1
    qt
    (spiFilter selPerm, annSQLBoolExp)
    mDistCols
  where
    selNecessaryMsg =
      "; \"count\" is only allowed if the role "
      <> "has \"select\" permissions on the table"
    relInDistColsErr =
      "Relationships can't be used in \"distinct\"."

countP2 :: (P2C m) => (CountQueryP1, DS.Seq Q.PrepArg) -> m RespBody
countP2 (u, p) = do
  qRes <- liftTx $ Q.rawQE dmlTxErrorHandler (Q.fromBuilder countSQL) (toList p) True
  return $ BB.toLazyByteString $ encodeCount qRes
  where
    countSQL = toSQL $ mkSQLCount u
    encodeCount (Q.SingleRow (Identity c)) =
      BB.byteString "{\"count\":" <> BB.intDec c <> BB.char7 '}'

instance HDBQuery CountQuery where

  type Phase1Res CountQuery = (CountQueryP1, DS.Seq Q.PrepArg)
  phaseOne = flip runStateT DS.empty . countP1 binRHSBuilder

  phaseTwo _ = countP2

  schemaCachePolicy = SCPNoChange
