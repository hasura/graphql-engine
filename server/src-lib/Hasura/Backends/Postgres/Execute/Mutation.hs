module Hasura.Backends.Postgres.Execute.Mutation
  ( MutationRemoteJoinCtx
  --
  , execDeleteQuery
  , execInsertQuery
  , execUpdateQuery
  --
  , executeMutationOutputQuery
  , mutateAndFetchCols
  ) where

import           Hasura.Prelude

import qualified Data.Environment                             as Env
import qualified Data.Sequence                                as DS
import qualified Database.PG.Query                            as Q
import qualified Network.HTTP.Client                          as HTTP
import qualified Network.HTTP.Types                           as N


import           Instances.TH.Lift                            ()

import qualified Hasura.Backends.Postgres.SQL.DML             as S
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.RemoteJoin
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.Delete
import           Hasura.Backends.Postgres.Translate.Insert
import           Hasura.Backends.Postgres.Translate.Mutation
import           Hasura.Backends.Postgres.Translate.Returning
import           Hasura.Backends.Postgres.Translate.Select
import           Hasura.Backends.Postgres.Translate.Update
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.Delete
import           Hasura.RQL.IR.Insert
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.IR.Select
import           Hasura.RQL.IR.Update
import           Hasura.RQL.Instances                         ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Server.Version                        (HasVersion)
import           Hasura.Session


type MutationRemoteJoinCtx = (HTTP.Manager, [N.Header], UserInfo)

data Mutation (b :: Backend)
  = Mutation
  { _mTable       :: !QualifiedTable
  , _mQuery       :: !(S.CTE, DS.Seq Q.PrepArg)
  , _mOutput      :: !(MutationOutput b)
  , _mCols        :: ![ColumnInfo b]
  , _mRemoteJoins :: !(Maybe (RemoteJoins b, MutationRemoteJoinCtx))
  , _mStrfyNum    :: !Bool
  }

mkMutation
  :: Maybe MutationRemoteJoinCtx
  -> QualifiedTable
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> MutationOutput 'Postgres
  -> [ColumnInfo 'Postgres]
  -> Bool
  -> Mutation 'Postgres
mkMutation ctx table query output' allCols strfyNum =
  let (output, remoteJoins) = getRemoteJoinsMutationOutput output'
      remoteJoinsCtx = (,) <$> remoteJoins <*> ctx
  in Mutation table query output allCols remoteJoinsCtx strfyNum

runMutation
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Mutation 'Postgres
  -> m EncJSON
runMutation env mut =
  bool (mutateAndReturn env mut) (mutateAndSel env mut) $
    hasNestedFld $ _mOutput mut

mutateAndReturn
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Mutation 'Postgres
  -> m EncJSON
mutateAndReturn env (Mutation qt (cte, p) mutationOutput allCols remoteJoins strfyNum) =
  executeMutationOutputQuery env sqlQuery (toList p) remoteJoins
  where
    sqlQuery = Q.fromBuilder $ toSQL $
               mkMutationOutputExp qt allCols Nothing cte mutationOutput strfyNum


execUpdateQuery
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Bool
  -> Maybe MutationRemoteJoinCtx
  -> (AnnUpd 'Postgres, DS.Seq Q.PrepArg)
  -> m EncJSON
execUpdateQuery env strfyNum remoteJoinCtx (u, p) =
  runMutation env $ mkMutation remoteJoinCtx (uqp1Table u) (updateCTE, p)
                (uqp1Output u) (uqp1AllCols u) strfyNum
  where
    updateCTE = mkUpdateCTE u

execDeleteQuery
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Bool
  -> Maybe MutationRemoteJoinCtx
  -> (AnnDel 'Postgres, DS.Seq Q.PrepArg)
  -> m EncJSON
execDeleteQuery env strfyNum remoteJoinCtx (u, p) =
  runMutation env $ mkMutation remoteJoinCtx (dqp1Table u) (deleteCTE, p)
                (dqp1Output u) (dqp1AllCols u) strfyNum
  where
    deleteCTE = mkDeleteCTE u

execInsertQuery
  :: ( HasVersion
     , MonadTx m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> Bool
  -> Maybe MutationRemoteJoinCtx
  -> (InsertQueryP1 'Postgres, DS.Seq Q.PrepArg)
  -> m EncJSON
execInsertQuery env strfyNum remoteJoinCtx (u, p) =
  runMutation env
     $ mkMutation remoteJoinCtx (iqp1Table u) (insertCTE, p)
                (iqp1Output u) (iqp1AllCols u) strfyNum
  where
    insertCTE = mkInsertCTE u



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
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Mutation 'Postgres
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
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Q.Query -- ^ SQL query
  -> [Q.PrepArg] -- ^ Prepared params
  -> Maybe (RemoteJoins 'Postgres, MutationRemoteJoinCtx)  -- ^ Remote joins context
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
  -> [ColumnInfo 'Postgres]
  -> (S.CTE, DS.Seq Q.PrepArg)
  -> Bool
  -> Q.TxE QErr (MutateResp TxtEncodedPGVal)
mutateAndFetchCols qt cols (cte, p) strfyNum =
  Q.getAltJ . runIdentity . Q.getRow
    -- See Note [Prepared statements in Mutations]
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) (toList p) False
  where
    aliasIdentifier = Identifier $ qualifiedObjectToText qt <> "__mutation_result"
    tabFrom = FromIdentifier aliasIdentifier
    tabPerm = TablePerm annBoolExpTrue Nothing
    selFlds = flip map cols $
              \ci -> (fromPGCol $ pgiColumn ci, mkAnnColumnFieldAsText ci)

    sql = toSQL selectWith
    selectWith = S.SelectWith [(S.Alias aliasIdentifier, cte)] select
    select = S.mkSelect {S.selExtr = [S.Extractor extrExp Nothing]}
    extrExp = S.applyJsonBuildObj
              [ S.SELit "affected_rows", affRowsSel
              , S.SELit "returning_columns", colSel
              ]

    affRowsSel = S.SESelect $
      S.mkSelect
      { S.selExtr = [S.Extractor S.countStar Nothing]
      , S.selFrom = Just $ S.FromExp [S.FIIdentifier aliasIdentifier]
      }
    colSel = S.SESelect $ mkSQLSelect JASMultipleRows $
             AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum
