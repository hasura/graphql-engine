{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- This module holds functions and data types used for logging at the GraphQL
-- layer. In contrast with, logging at the HTTP server layer.
module Hasura.GraphQL.Logging
  ( QueryLog (..),
    ExecutionStats (..),
    statsToAnyBackend,
    GeneratedQuery (..),
    MonadQueryLog (..),
    QueryLogKind (..),
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.Kind (Type)
import Data.Text.Extended
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Execute.Backend (ActionResult (..))
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.ConnectionTemplate (BackendResolvedConnectionTemplate (..))
import Hasura.RQL.Types.Backend (Backend (ExecutionStatistics))
import Hasura.SQL.AnyBackend (AnyBackend, dispatchAnyBackend', mkAnyBackend)
import Hasura.SQL.Backend (BackendType)
import Hasura.SQL.Tag (HasTag)
import Hasura.Server.Types (RequestId)
import Hasura.Tracing (TraceT)

-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog = QueryLog
  { _qlQuery :: !GQLReqUnparsed,
    _qlGeneratedSql :: !(Maybe (RootFieldAlias, GeneratedQuery)),
    _qlRequestId :: !RequestId,
    _qlKind :: !QueryLogKind,
    _qlStatistics :: !(Maybe (AnyBackend ExecutionStats))
  }

-- | 'ExecutionStatistics' is a type family, which means we can't partially
-- apply it (in 'AnyBackend', for example). To get round this, we have a
-- newtype that really just wraps the type family.
type ExecutionStats :: BackendType -> Type
newtype ExecutionStats b = ExecutionStats (ExecutionStatistics b)

-- | When we want to log anything from 'DBStepInfo', we first need to transform
-- the backend-specific execution statistics into 'AnyBackend' statistics. This
-- is fine in practice because all we do with it is log it as JSON.
statsToAnyBackend :: forall b. (HasTag b) => ActionResult b -> (Maybe (AnyBackend ExecutionStats), EncJSON)
statsToAnyBackend ActionResult {..} =
  (fmap (mkAnyBackend @b . ExecutionStats) arStatistics, arResult)

deriving newtype instance Backend b => J.ToJSON (ExecutionStats b)

data QueryLogKind
  = QueryLogKindDatabase (Maybe (BackendResolvedConnectionTemplate))
  | QueryLogKindDatabaseResponse (Maybe (BackendResolvedConnectionTemplate))
  | QueryLogKindAction
  | QueryLogKindRemoteSchema
  | QueryLogKindCached
  | QueryLogKindIntrospection

instance J.ToJSON QueryLogKind where
  toJSON = \case
    QueryLogKindDatabase _ -> "database"
    QueryLogKindDatabaseResponse _ -> "database-response"
    QueryLogKindAction -> "action"
    QueryLogKindRemoteSchema -> "remote-schema"
    QueryLogKindCached -> "cached"
    QueryLogKindIntrospection -> "introspection"

data GeneratedQuery = GeneratedQuery
  { _gqQueryString :: Text,
    _gqPreparedArgs :: J.Value
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog gqlQuery generatedQuery reqId kind mstatistics) =
    J.object $
      [ "query" J..= gqlQuery,
        -- NOTE: this customizes the default JSON instance of a pair
        "generated_sql" J..= fmap fromPair generatedQuery,
        "request_id" J..= reqId,
        "kind" J..= kind,
        "statistics" J..= case mstatistics of
          Just statistics -> dispatchAnyBackend' @J.ToJSON statistics J.toJSON
          Nothing -> J.toJSON ()
      ]
        <> maybe [] (\val -> ["connection_template" J..= val]) (getResolvedConnectionTemplate kind)
    where
      fromPair p = Map.fromList [first toTxt p]
      getResolvedConnectionTemplate :: QueryLogKind -> Maybe (BackendResolvedConnectionTemplate)
      getResolvedConnectionTemplate (QueryLogKindDatabase x) = x
      getResolvedConnectionTemplate _ = Nothing

instance J.ToJSON GeneratedQuery where
  toJSON (GeneratedQuery queryString preparedArgs) =
    J.object
      [ "query" J..= queryString,
        "prepared_arguments" J..= preparedArgs
      ]

instance L.ToEngineLog QueryLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)

class Monad m => MonadQueryLog m where
  logQueryLog ::
    L.Logger L.Hasura ->
    QueryLog ->
    m ()

instance MonadQueryLog m => MonadQueryLog (ExceptT e m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance MonadQueryLog m => MonadQueryLog (ReaderT r m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance MonadQueryLog m => MonadQueryLog (TraceT m) where
  logQueryLog logger l = lift $ logQueryLog logger l
