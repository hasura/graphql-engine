module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , mutateAndFetchCols
  , execCTEAndBuildMutResp
  )
where

import qualified Data.Sequence                          as DS

import           Hasura.GraphQL.Transport.HTTP.Protocol (mkJSONObj)
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances                   ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Hasura.SQL.DML                         as S

data Mutation
  = Mutation
  { _mTable    :: !QualifiedTable
  , _mQuery    :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mFields   :: !MutFlds
  , _mUniqCols :: !(Maybe [PGColInfo])
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr RespBody
runMutation mut =
  bool (mutateAndReturn qSingleObj mut) (mutateAndSel qSingleObj mut) $
    hasNestedFld $ _mFields mut
  where
    qSingleObj = QuerySingleObj False

mutateAndReturn :: QuerySingleObj -> Mutation -> Q.TxE QErr RespBody
mutateAndReturn qSingleObj (Mutation qt cte mutFlds _ strfyNum) =
  execCTEAndBuildMutResp qt cte mutFlds qSingleObj strfyNum

mutateAndSel :: QuerySingleObj -> Mutation -> Q.TxE QErr RespBody
mutateAndSel qSingleObj (Mutation qt q mutFlds mUniqCols strfyNum) = do
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
  -- Perform select query and fetch returning fields
  execCTEAndBuildMutResp qt (selCTE, DS.empty) mutFlds qSingleObj strfyNum
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
mutateAndFetchCols qt cols cte strfyNum = do
  res <- execCTEAndBuildMutResp qt cte mutFlds qSingleObj strfyNum
  decodeFromBS res
  where
    qSingleObj = QuerySingleObj False
    mutFlds = [ ("affected_rows", MCount)
              , ("returning_columns", MRet selFlds)
              ]
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiName ci, FCol ci)

execCTEAndBuildMutResp
  :: QualifiedTable
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> MutFlds
  -> QuerySingleObj
  -> Bool
  -> Q.TxE QErr RespBody
execCTEAndBuildMutResp qt (cte, p) mutFlds singleObj strfyNum = do
  mutResults <-
    Q.listQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  queryResults <- forM qFlds $ \(t, qTx) -> do
    r <- qTx
    return (t, r)
  let resMap = Map.fromList mutResults
               `Map.union` Map.fromList queryResults
  mutResp <- forM mutFlds $ \(t, _) -> do
    r <- onNothing (Map.lookup t resMap) $
      throw500 $ "alias " <> t <> " not found in results"
    return (t, r)
  return $ mkJSONObj mutResp
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
