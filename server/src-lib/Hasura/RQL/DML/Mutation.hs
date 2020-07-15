module Hasura.RQL.DML.Mutation
  ( Mutation
  , mkMutation
  , MutationRemoteJoinCtx
  , runMutation
  , executeMutationOutputQuery
  , mutateAndFetchCols
  , mkSelCTEFromColVals
  )
where

import           Hasura.Prelude

import qualified Data.Environment          as Env
import qualified Data.HashMap.Strict       as Map
import qualified Data.Sequence             as DS
import qualified Database.PG.Query         as Q
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Types        as N

import qualified Hasura.SQL.DML            as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.RemoteJoin
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Instances      ()
import           Hasura.RQL.Types
import           Hasura.Server.Version     (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type MutationRemoteJoinCtx = (HTTP.Manager, [N.Header], UserInfo)

data Mutation
  = Mutation
  { _mTable       :: !QualifiedTable
  , _mQuery       :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mOutput      :: !MutationOutput
  , _mCols        :: ![PGColumnInfo]
  , _mRemoteJoins :: !(Maybe (RemoteJoins, MutationRemoteJoinCtx))
  , _mStrfyNum    :: !Bool
  }

mkMutation
  :: Maybe MutationRemoteJoinCtx
  -> QualifiedTable
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> MutationOutput
  -> [PGColumnInfo]
  -> Bool
  -> Mutation
mkMutation ctx table query output' allCols strfyNum =
  let (output, remoteJoins) = getRemoteJoinsMutationOutput output'
      remoteJoinsCtx = (,) <$> remoteJoins <*> ctx
  in Mutation table query output allCols remoteJoinsCtx strfyNum

runMutation
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  )
  => Env.Environment
  -> Mutation
  -> m EncJSON
runMutation env mut =
  bool (mutateAndReturn env mut) (mutateAndSel env mut) $
    hasNestedFld $ _mOutput mut

mutateAndReturn
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  )
  => Env.Environment
  -> Mutation
  -> m EncJSON
mutateAndReturn env (Mutation qt (cte, p) mutationOutput allCols remoteJoins strfyNum) =
  executeMutationOutputQuery env sqlQuery (toList p) remoteJoins
  where
    sqlQuery = Q.fromBuilder $ toSQL $
               mkMutationOutputExp qt allCols Nothing cte mutationOutput strfyNum

{- Note: [Prepared statements in Mutations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The SQL statements we generate for mutations seem to include the actual values
in the statements in some cases which pretty much makes them unfit for reuse
(Handling relationships in the returning clause is the source of this
complexity). Further, `PGConn` has an internal cache which maps a statement to
a 'prepared statement id' on Postgres. As we prepare more and more single-use
SQL statements we end up leaking memory both on graphql-engine and Postgres
till the connection is closed. So a simpler but very crude fix is to not use
prepared statements for mutations. The performance of insert mutations
shouldn't be affected but updates and delete mutations with complex boolean
conditions **might** see some degradation.
-}

mutateAndSel
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  )
  => Env.Environment
  -> Mutation
  -> m EncJSON
mutateAndSel env (Mutation qt q mutationOutput allCols remoteJoins strfyNum) = do
  -- Perform mutation and fetch unique columns
  MutateResp _ columnVals <- liftTx $ mutateAndFetchCols qt allCols q strfyNum
  selCTE <- mkSelCTEFromColVals qt allCols columnVals
  let selWith = mkMutationOutputExp qt allCols Nothing selCTE mutationOutput strfyNum
  -- Perform select query and fetch returning fields
  executeMutationOutputQuery env (Q.fromBuilder $ toSQL selWith) [] remoteJoins

executeMutationOutputQuery
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  )
  => Env.Environment
  -> Q.Query -- ^ SQL query
  -> [Q.PrepArg] -- ^ Prepared params
  -> Maybe (RemoteJoins, MutationRemoteJoinCtx)  -- ^ Remote joins context
  -> m EncJSON
executeMutationOutputQuery env query prepArgs = \case
  Nothing ->
    runIdentity . Q.getRow
      -- See Note [Prepared statements in Mutations]
      <$> liftTx (Q.rawQE dmlTxErrorHandler query prepArgs False)
  Just (remoteJoins, (httpManager, reqHeaders, userInfo)) ->
    executeQueryWithRemoteJoins env httpManager reqHeaders userInfo query prepArgs remoteJoins


mutateAndFetchCols
  :: QualifiedTable
  -> [PGColumnInfo]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr (MutateResp TxtEncodedPGVal)
mutateAndFetchCols qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    -- See Note [Prepared statements in Mutations]
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) False
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
