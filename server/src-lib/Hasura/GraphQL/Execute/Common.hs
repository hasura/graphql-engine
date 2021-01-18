module Hasura.GraphQL.Execute.Common
  where

-- Code shared between Hasura.GraphQL.Execute.Query and .Mutation

import           Hasura.Prelude

import qualified Data.Aeson                                  as J
import qualified Data.Environment                            as Env
import qualified Data.IntMap                                 as IntMap
import qualified Database.PG.Query                           as Q
import qualified Network.HTTP.Client                         as HTTP
import qualified Network.HTTP.Types                          as HTTP

import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as DS
import qualified Hasura.RQL.IR.Select                        as DS
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.RemoteJoin
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.Select   (asSingleRowJsonResp)
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.RQL.Types
import           Hasura.Server.Version                       (HasVersion)
import           Hasura.Session

data PreparedSql
  = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: !PrepArgMap
  , _psRemoteJoins :: !(Maybe (RemoteJoins 'Postgres))
  }

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs _) =
    J.object [ "query" J..= Q.getQueryText q
             , "prepared_arguments" J..= fmap (pgScalarValueToJson . snd) prepArgs
             ]

data RootFieldPlan
  = RFPPostgres !PreparedSql
  -- | RFPActionQuery !ActionExecution

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPPostgres pgPlan -> J.toJSON pgPlan
    -- RFPActionQuery _   -> J.String "Action Execution Tx"


-- | A method for extracting profiling data from instrumented query results.
newtype ExtractProfile = ExtractProfile
  { runExtractProfile :: forall m. (MonadIO m, Tracing.MonadTrace m) => EncJSON -> m EncJSON
  }

-- turn the current plan into a transaction
mkCurPlanTx
  :: ( HasVersion
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> (Q.Query -> Q.Query)
  -> ExtractProfile
  -> RootFieldPlan
  -> (tx EncJSON, Maybe PreparedSql)
mkCurPlanTx env manager reqHdrs userInfo instrument ep = \case
  -- generate the SQL and prepared vars or the bytestring
    RFPPostgres ps@(PreparedSql q prepMap remoteJoinsM) ->
      let args = withUserVars (_uiSession userInfo) prepMap
          -- WARNING: this quietly assumes the intmap keys are contiguous
          prepArgs = fst <$> IntMap.elems args
      in (, Just ps) $ case remoteJoinsM of
           Nothing -> do
             Tracing.trace "Postgres" $ runExtractProfile ep =<< liftTx do
               asSingleRowJsonResp (instrument q) prepArgs
           Just remoteJoins ->
             executeQueryWithRemoteJoins env manager reqHdrs userInfo q prepArgs remoteJoins
    -- RFPActionQuery atx -> (unActionExecution atx, Nothing)

-- convert a query from an intermediate representation to... another
irToRootFieldPlan
  :: PrepArgMap
  -> QueryDB 'Postgres S.SQLExp -> PreparedSql
irToRootFieldPlan prepped = \case
  QDBSimple s      -> mkPreparedSql getRemoteJoins (DS.selectQuerySQL DS.JASMultipleRows) s
  QDBPrimaryKey s  -> mkPreparedSql getRemoteJoins (DS.selectQuerySQL DS.JASSingleObject) s
  QDBAggregation s -> mkPreparedSql getRemoteJoinsAggregateSelect DS.selectAggregateQuerySQL s
  QDBConnection s  -> mkPreparedSql getRemoteJoinsConnectionSelect DS.connectionSelectQuerySQL s
  where
    mkPreparedSql :: (s -> (t, Maybe (RemoteJoins 'Postgres))) -> (t -> Q.Query) -> s -> PreparedSql
    mkPreparedSql getJoins f simpleSel =
      let (simpleSel',remoteJoins) = getJoins simpleSel
      in PreparedSql (f simpleSel') prepped remoteJoins

-- | A default implementation for queries with no instrumentation
noProfile :: ExtractProfile
noProfile = ExtractProfile pure
