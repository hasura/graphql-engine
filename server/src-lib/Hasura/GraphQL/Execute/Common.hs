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
import qualified Network.Wai.Extended                        as Wai

import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as DS
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.RemoteJoin
import           Hasura.Backends.Postgres.Translate.Select   (asSingleRowJsonResp)
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.RemoteJoin
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                       (HasVersion)
import           Hasura.Session


data PreparedSql pgKind
  = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: !PrepArgMap
  , _psRemoteJoins :: !(Maybe (RemoteJoins ('Postgres pgKind)))
  }

-- | Typeclass representing safety checks (if any) that need to be performed
-- before a GraphQL query should be allowed to be executed. In OSS, the safety
-- check is to check in the query is in the allow list.
--
-- the `executeIntrospection` function has different implementations in OSS and
-- Pro. In Pro, the GraphQL schema introspection can be disabled for specified
-- roles and in OSS there is no restrictions.
--
-- | TODO (from master): Limitation: This parses the query, which is not ideal if we already
-- have the query cached. The parsing happens unnecessary. But getting this to
-- either return a plan or parse was tricky and complicated.
class Monad m => MonadGQLExecutionCheck m where
  checkGQLExecution
    :: UserInfo
    -> ([HTTP.Header], Wai.IpAddress)
    -> Bool
    -- ^ allow list enabled?
    -> SchemaCache
    -- ^ needs allow list
    -> GQLReqUnparsed
    -- ^ the unparsed GraphQL query string (and related values)
    -> m (Either QErr GQLReqParsed)

  executeIntrospection
    :: UserInfo
    -> J.Value
    -> SetGraphqlIntrospectionOptions
    -> m (Either QErr ExecutionStep)

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ExceptT e m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ReaderT r m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (Tracing.TraceT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (MetadataStorageT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

-- turn the current plan into a transaction
mkCurPlanTx
  :: ( HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> PreparedSql pgKind
  -> (Tracing.TraceT (LazyTxT QErr IO) EncJSON, Maybe (PreparedSql pgKind))
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
  :: Backend ('Postgres pgKind)
  => PrepArgMap
  -> QueryDB ('Postgres pgKind) S.SQLExp
  -> PreparedSql pgKind
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql getRemoteJoinsSelect (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s    -> mkPreparedSql getRemoteJoinsSelect (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s  -> mkPreparedSql getRemoteJoinsAggregateSelect DS.selectAggregateQuerySQL s
  QDBConnection s   -> mkPreparedSql getRemoteJoinsConnectionSelect DS.connectionSelectQuerySQL s
  where
    mkPreparedSql :: (s -> (t, Maybe (RemoteJoins ('Postgres pgKind)))) -> (t -> Q.Query) -> s -> PreparedSql pgKind
    mkPreparedSql getJoins f simpleSel =
      let (simpleSel',remoteJoins) = getJoins simpleSel
      in PreparedSql (f simpleSel') prepped remoteJoins
