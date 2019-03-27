module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , mutateAndFetchCols
  , execCTEAndBuildMutResp
  , mkSelCTEFromColVals
  )
where

import qualified Data.Sequence            as DS

import           Hasura.EncJSON
import           Hasura.Prelude
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
  , _mCols     :: ![PGColInfo]
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr EncJSON
runMutation mut =
  bool (mutateAndReturn qSingleObj mut) (mutateAndSel qSingleObj mut) $
    hasNestedFld $ _mFields mut
  where
    qSingleObj = QuerySingleObj False

mutateAndReturn :: QuerySingleObj -> Mutation -> Q.TxE QErr EncJSON
mutateAndReturn qSingleObj (Mutation qt cte mutFlds _ strfyNum) =
  execCTEAndBuildMutResp qt cte mutFlds qSingleObj strfyNum

mutateAndSel :: QuerySingleObj -> Mutation -> Q.TxE QErr EncJSON
mutateAndSel qSingleObj (Mutation qt q mutFlds allCols strfyNum) = do
  -- Perform mutation and fetch unique columns
  MutateResp _ colVals <- mutateAndFetchCols qt allCols q strfyNum
  selCTE <- mkSelCTEFromColVals qt allCols colVals
  -- Perform select query and fetch returning fields
  execCTEAndBuildMutResp qt (selCTE, DS.empty) mutFlds qSingleObj False

mutateAndFetchCols
  :: QualifiedTable
  -> [PGColInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr MutateResp
mutateAndFetchCols qt cols cte strfyNum = do
  res <- execCTEAndBuildMutResp qt cte mutFlds qSingleObj strfyNum
  decodeEncJSON res
  where
    qSingleObj = QuerySingleObj False
    mutFlds = [ ("affected_rows", MCount)
              , ("returning_columns", MRet selFlds)
              ]
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiName ci, FCol ci Nothing)

mkSelCTEFromColVals
  :: MonadError QErr m
  => QualifiedTable -> [PGColInfo] -> [ColVals] -> m S.CTE
mkSelCTEFromColVals qt allCols colVals =
  S.CTESelect <$> case colVals of
    [] -> return selNoRows
    _  -> do
      tuples <- mapM mkTupsFromColVal colVals
      let fromItem = S.FIValues (S.ValuesExp tuples) tableAls $ Just colNames
      return S.mkSelect
        { S.selExtr = [S.selectStar]
        , S.selFrom = Just $ S.FromExp [fromItem]
        }
  where
    tableAls = S.Alias $ Iden $ snakeCaseQualObject qt
    colNames = map pgiName allCols
    mkTupsFromColVal colVal =
      fmap S.TupleExp $ forM allCols $ \ci -> do
        let pgCol = pgiName ci
        val <- onNothing (Map.lookup pgCol colVal) $
          throw500 $ "column " <> pgCol <<> " not found in returning values"
        runAesonParser (convToTxt (pgiType ci)) val

    selNoRows =
      S.mkSelect { S.selExtr = [S.selectStar]
                 , S.selFrom = Just $ S.mkSimpleFromExp qt
                 , S.selWhere = Just $ S.WhereFrag $ S.BELit False
                 }

execCTEAndBuildMutResp
  :: QualifiedTable
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> MutFlds
  -> QuerySingleObj
  -> Bool
  -> Q.TxE QErr EncJSON
execCTEAndBuildMutResp qt (cte, p) mutFlds singleObj strfyNum = do
  mutResults <-
    Q.listQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  queryResults <- forM qFlds $ \(t, qTx) -> do
    r <- qTx
    return (t, encJToLBS r)
  let resMap = Map.fromList mutResults
               `Map.union` Map.fromList queryResults
  mutResp <- forM mutFlds $ \(t, _) -> do
    r <- onNothing (Map.lookup t resMap) $
      throw500 $ "alias " <> t <> " not found in results"
    return (t, encJFromLBS r)
  return $ encJFromAssocList mutResp
  where
    sql = toSQL selWith
    alias = Iden $ snakeCaseTable qt <> "__mutation_result_alias"
    selWith = S.SelectWith [(S.Alias alias, cte)] unionSelects
    unionSelects = countSels <> expSels <> returningSels

    mkExtrs t e =
      let aliasIden = S.Alias $ Iden "alias"
          aliasLit = S.SELit t
          resultIden = S.Alias $ Iden "result"
      in [ S.Extractor aliasLit (Just aliasIden)
         , S.Extractor e (Just resultIden)
         ]

    countFlds = mapMaybe getCount mutFlds
    countSels = flip map countFlds $ \t ->
      S.mkSelect
      { S.selExtr = mkExtrs t $ S.sqlToJSON S.countStar
      , S.selFrom = Just $ S.FromExp $ pure $ S.FIIden alias
      }

    expFlds = mapMaybe getExp mutFlds
    expSels = flip map expFlds $ \(t, e) ->
      S.mkSelect {S.selExtr = mkExtrs t $ S.sqlToJSON $ S.SELit e}

    returningFlds = mapMaybe getRetFld mutFlds
    returningSels = flip map returningFlds $ \(t, selFlds) ->
      let tabFrom = TableFrom qt $ Just alias
          tabPerm = TablePerm annBoolExpTrue Nothing
          selExp = S.SESelect $ mkSQLSelect singleObj $
            AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum
      in S.mkSelect {S.selExtr = mkExtrs t selExp}

    qFlds = mapMaybe getQFld mutFlds

    getCount (t, fld) =
      case fld of
        MCount -> Just t
        _      -> Nothing

    getExp (t, fld) =
      case fld of
        (MExp e) -> Just (t, e)
        _        -> Nothing

    getRetFld (t, fld) =
      case fld of
        (MRet r) -> Just (t, r)
        _        -> Nothing

    getQFld (t, fld) =
      case fld of
        (MQuery q) -> Just (t, q)
        _          -> Nothing
