module Hasura.GraphQL.Execute.Common
  where

-- Code shared between Hasura.GraphQL.Execute.Query and .Mutation

import           Hasura.Prelude

import qualified Data.Environment                            as Env
import qualified Data.IntMap                                 as IntMap
import qualified Database.PG.Query                           as Q
import qualified Network.HTTP.Client                         as HTTP
import qualified Network.HTTP.Types                          as HTTP

import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as DS
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.RemoteJoin
import           Hasura.Backends.Postgres.Translate.Select   (asSingleRowJsonResp)
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.RemoteJoin
import           Hasura.RQL.Types
import           Hasura.Server.Version                       (HasVersion)
import           Hasura.Session


data PreparedSql
  = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: !PrepArgMap
  , _psRemoteJoins :: !(Maybe (RemoteJoins 'Postgres))
  }


-- turn the current plan into a transaction
mkCurPlanTx
  :: ( HasVersion
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> PreparedSql
  -> (Tracing.TraceT (LazyTxT QErr IO) EncJSON, Maybe PreparedSql)
mkCurPlanTx env manager reqHdrs userInfo ps@(PreparedSql q prepMap remoteJoinsM) =
  -- generate the SQL and prepared vars or the bytestring
  let args = withUserVars (_uiSession userInfo) prepMap
      -- WARNING: this quietly assumes the intmap keys are contiguous
      prepArgs = fst <$> IntMap.elems args
  in (, Just ps) $ case remoteJoinsM of
    Nothing -> do
      Tracing.trace "Postgres" $ liftTx $ asSingleRowJsonResp q prepArgs
    Just remoteJoins ->
      executeQueryWithRemoteJoins env manager reqHdrs userInfo q prepArgs remoteJoins

-- convert a query from an intermediate representation to... another
irToRootFieldPlan
  :: PrepArgMap
  -> QueryDB 'Postgres S.SQLExp
  -> PreparedSql
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql getRemoteJoinsSelect (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s    -> mkPreparedSql getRemoteJoinsSelect (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s  -> mkPreparedSql getRemoteJoinsAggregateSelect DS.selectAggregateQuerySQL s
  QDBConnection s   -> mkPreparedSql getRemoteJoinsConnectionSelect DS.connectionSelectQuerySQL s
  where
    mkPreparedSql :: (s -> (t, Maybe (RemoteJoins 'Postgres))) -> (t -> Q.Query) -> s -> PreparedSql
    mkPreparedSql getJoins f simpleSel =
      let (simpleSel',remoteJoins) = getJoins simpleSel
      in PreparedSql (f simpleSel') prepped remoteJoins
