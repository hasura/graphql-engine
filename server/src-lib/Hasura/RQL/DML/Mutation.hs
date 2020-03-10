module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , mutateAndFetchCols
  , mkSelCTEFromColVals
  )
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict            as Map
import qualified Data.Sequence                  as DS
import qualified Database.PG.Query              as Q

import qualified Hasura.SQL.DML                 as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances           ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data Mutation
  = Mutation
  { _mTable    :: !QualifiedTable
  , _mQuery    :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mOutput   :: !MutationOutput
  , _mCols     :: ![PGColumnInfo]
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr EncJSON
runMutation mut =
  bool (mutateAndReturn mut) (mutateAndSel mut) $
    hasNestedFld $ _mOutput mut

mutateAndReturn :: Mutation -> Q.TxE QErr EncJSON
mutateAndReturn (Mutation qt (cte, p) mutationOutput allCols strfyNum) =
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith)
        (toList p) True
  where
    selWith = mkMutationOutputExp qt allCols Nothing cte mutationOutput strfyNum

mutateAndSel :: Mutation -> Q.TxE QErr EncJSON
mutateAndSel (Mutation qt q mutationOutput allCols strfyNum) = do
  -- Perform mutation and fetch unique columns
  MutateResp _ columnVals <- mutateAndFetchCols qt allCols q strfyNum
  selCTE <- mkSelCTEFromColVals qt allCols columnVals
  let selWith = mkMutationOutputExp qt allCols Nothing selCTE mutationOutput strfyNum
  -- Perform select query and fetch returning fields
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder $ toSQL selWith) [] True


mutateAndFetchCols
  :: QualifiedTable
  -> [PGColumnInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr (MutateResp TxtEncodedPGVal)
mutateAndFetchCols qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  where
    aliasIden = Iden $ qualObjectToText qt <> "__mutation_result"
    tabFrom = FromIden aliasIden
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiColumn ci, mkAnnColumnFieldAsText ci)

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
    colSel = S.SESelect $ mkSQLSelect JASMultipleRows $
             AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum

-- | Note:- Using sorted columns is necessary to enable casting the rows returned by VALUES expression to table type.
-- For example, let's consider the table, `CREATE TABLE test (id serial primary key, name text not null, age int)`.
-- The generated values expression should be in order of columns;
-- `SELECT ("row"::table).* VALUES (1, 'Robert', 23) AS "row"`.
mkSelCTEFromColVals
  :: (MonadError QErr m)
  => QualifiedTable -> [PGColumnInfo] -> [ColumnValues TxtEncodedPGVal] -> m S.CTE
mkSelCTEFromColVals qt allCols colVals =
  S.CTESelect <$> case colVals of
    [] -> return selNoRows
    _  -> do
      tuples <- mapM mkTupsFromColVal colVals
      let fromItem = S.FIValues (S.ValuesExp tuples) (S.Alias rowAlias) Nothing
      return S.mkSelect
        { S.selExtr = [extractor]
        , S.selFrom = Just $ S.FromExp [fromItem]
        }
  where
    rowAlias = Iden "row"
    extractor = S.selectStar' $ S.QualIden rowAlias $ Just $ S.TypeAnn $ toSQLTxt qt
    sortedCols = sortCols allCols
    mkTupsFromColVal colVal =
      fmap S.TupleExp $ forM sortedCols $ \ci -> do
        let pgCol = pgiColumn ci
        val <- onNothing (Map.lookup pgCol colVal) $
          throw500 $ "column " <> pgCol <<> " not found in returning values"
        pure $ txtEncodedToSQLExp (pgiType ci) val

    selNoRows =
      S.mkSelect { S.selExtr = [S.selectStar]
                 , S.selFrom = Just $ S.mkSimpleFromExp qt
                 , S.selWhere = Just $ S.WhereFrag $ S.BELit False
                 }

    txtEncodedToSQLExp colTy = \case
      TENull          -> S.SENull
      TELit textValue ->
        S.withTyAnn (unsafePGColumnToRepresentation colTy) $ S.SELit textValue
