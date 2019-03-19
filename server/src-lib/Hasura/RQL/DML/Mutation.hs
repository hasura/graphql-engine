module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , mutateAndFetchCols
  )
where

import qualified Data.Sequence            as DS

import           Hasura.Prelude
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Data.HashMap.Strict      as Map
import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data Mutation
  = Mutation
  { _mTable    :: !QualifiedTable
  , _mQuery    :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mFields   :: !MutFlds
  , _mUniqCols :: !(Maybe [PGColInfo])
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr EncJSON
runMutation mut =
  bool (mutateAndReturn mut) (mutateAndSel mut) $
    hasNestedFld $ _mFields mut

mutateAndReturn :: Mutation -> Q.TxE QErr EncJSON
mutateAndReturn (Mutation qt (cte, p) mutFlds _ strfyNum) =
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith)
        (toList p) True
  where
    selWith = mkSelWith qt cte mutFlds False strfyNum

mutateAndSel :: Mutation -> Q.TxE QErr EncJSON
mutateAndSel (Mutation qt q mutFlds mUniqCols strfyNum) = do
  uniqCols <- onNothing mUniqCols $
              throw500 "uniqCols not found in mutateAndSel"
  let colMap = Map.fromList $ flip map uniqCols $
               \ci -> (pgiName ci, ci)
  -- Perform mutation and fetch unique columns
  MutateResp _ colVals <- mutateAndFetchCols qt uniqCols q strfyNum
  colExps <- mapM (colValToColExp colMap) colVals
  let selWhere = S.mkBoolExpWithColVal mkQIdenExp colExps
      selCTE = S.CTESelect $
               S.mkSelect
               { S.selExtr = [S.selectStar]
               , S.selFrom = Just $ S.FromExp [S.FISimple qt Nothing]
               , S.selWhere = Just $ S.WhereFrag selWhere
               }
      selWith = mkSelWith qt selCTE mutFlds False strfyNum
  -- Perform select query and fetch returning fields
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith) [] True
  where
    colValToColExp colMap colVal =
      fmap Map.fromList $ forM (Map.toList colVal) $
      \(pgCol, val) -> do
        colInfo <- onNothing (Map.lookup pgCol colMap) $
          throw500 "colInfo not found; colValToColExp"
        sqlExp <- runAesonParser (convToTxt (pgiType colInfo)) val
        return (pgCol, sqlExp)

    mkQIdenExp col =
      S.SEQIden $ S.QIden (S.mkQual qt) $ Iden $ getPGColTxt col


mutateAndFetchCols
  :: QualifiedTable
  -> [PGColInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr MutateResp
mutateAndFetchCols qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  where
    aliasIden = Iden $ qualObjectToText qt <> "__mutation_result"
    tabFrom = TableFrom qt $ Just aliasIden
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiName ci, FCol ci)

    sql = toSQL selectWith
    selectWith = S.SelectWith [(S.Alias aliasIden, cte)] select
    select = S.mkSelect {S.selExtr = [S.Extractor extrExp Nothing]}
    extrExp = S.applyJsonBuildObj
              [ S.SELit "affected_rows", affRowsSel
              , S.SELit "returning_columns", colSel
              ]

    affRowsSel = S.SESelect $
      S.mkSelect
      { S.selExtr = [S.Extractor S.countStar Nothing]
      , S.selFrom = Just $ S.FromExp [S.FIIden aliasIden]
      }
    colSel = S.SESelect $ mkSQLSelect False $
             AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum
