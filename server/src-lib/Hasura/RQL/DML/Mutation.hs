module Hasura.RQL.DML.Mutation
  ( Mutation(..)
  , runMutation
  , runMutationWith
  , mutateAndFetchCols
  , mkSelCTEFromColVals
  )
where

import           Data.Aeson
import           Hasura.Prelude

import qualified Data.HashMap.Strict      as Map
import qualified Data.Sequence            as DS
import qualified Database.PG.Query        as Q

import qualified Hasura.SQL.DML           as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data Mutation
  = Mutation
  { _mTable    :: !QualifiedTable
  , _mQuery    :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mFields   :: !MutFlds
  , _mCols     :: ![PGColumnInfo]
  , _mStrfyNum :: !Bool
  } deriving (Show, Eq)

runMutation :: Mutation -> Q.TxE QErr EncJSON
runMutation = runMutationWith id
    
runMutationWith 
  :: (S.SelectWith -> S.SelectWith) 
  -> Mutation 
  -> Q.TxE QErr EncJSON
runMutationWith f mut =
  (bool mutateAndReturn mutateAndSel)
    (hasNestedFld (_mFields mut)) f mut

mutateAndReturn 
  :: (S.SelectWith -> S.SelectWith) 
  -> Mutation 
  -> Q.TxE QErr EncJSON
mutateAndReturn f = mutateWith f (pure . _mQuery)

mutateAndSel 
  :: (S.SelectWith -> S.SelectWith) 
  -> Mutation 
  -> Q.TxE QErr EncJSON
mutateAndSel f = mutateWith id $ \(Mutation qt q _ allCols strfyNum) -> do
    -- Perform mutation and fetch unique columns
    MutateResp _ columnVals <- mutateAndFetchCols f qt allCols q strfyNum
    (, mempty) <$> mkSelCTEFromColVals txtEncodedToSQLExp qt allCols columnVals
  where
    txtEncodedToSQLExp colTy = pure . \case
      TENull          -> S.SENull
      TELit textValue ->
        S.withTyAnn (unsafePGColumnToRepresentation colTy) $ S.SELit textValue

mutateWith 
  :: (S.SelectWith -> S.SelectWith) 
  -> (Mutation -> Q.TxE QErr (S.CTE, DS.Seq Q.PrepArg)) 
  -> Mutation 
  -> Q.TxE QErr EncJSON
mutateWith f g m@(Mutation qt _ mutFlds _ strfyNum) = do
  (cte, args) <- g m
  let selWith = mkSelWith qt cte mutFlds False strfyNum
  -- Perform select query and fetch returning fields
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder . toSQL $ f selWith) (toList args) True

mutateAndFetchCols
  :: (FromJSON a)
  => (S.SelectWith -> S.SelectWith) 
  -> QualifiedTable
  -> [PGColumnInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr (MutateResp a)
mutateAndFetchCols f qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) True
  where
    aliasIden = qualTableToAliasIden qt
    tabFrom = FromIden aliasIden
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiColumn ci, mkAnnColFieldAsText ci)

    sql = toSQL (f selectWith)
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

mkSelCTEFromColVals
  :: (MonadError QErr m)
  => (PGColumnType -> a -> m S.SQLExp)
  -> QualifiedTable -> [PGColumnInfo] -> [ColumnValues a] -> m S.CTE
mkSelCTEFromColVals parseFn qt allCols colVals =
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
    colNames = map pgiColumn allCols
    mkTupsFromColVal colVal =
      fmap S.TupleExp $ forM allCols $ \ci -> do
        let pgCol = pgiColumn ci
        val <- onNothing (Map.lookup pgCol colVal) $
          throw500 $ "column " <> pgCol <<> " not found in returning values"
        parseFn (pgiType ci) val

    selNoRows =
      S.mkSelect { S.selExtr = [S.selectStar]
                 , S.selFrom = Just $ S.mkSimpleFromExp qt
                 , S.selWhere = Just $ S.WhereFrag $ S.BELit False
                 }
