-- |
-- This module holds functions and data types used for logging at the GraphQL
-- layer. In contrast with, logging at the HTTP server layer.
module Hasura.GraphQL.Logging.QueryLog
  ( QueryLog (..),
    GeneratedQuery (..),
    MonadQueryLog (..),
    QueryLogKind (..),
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.ConnectionTemplate (BackendResolvedConnectionTemplate (..))
import Hasura.Server.Types (RequestId)
import Hasura.Tracing (TraceT)

-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog = QueryLog
  { _qlQuery :: !GQLReqUnparsed,
    _qlGeneratedSql :: !(Maybe (RootFieldAlias, GeneratedQuery)),
    _qlRequestId :: !RequestId,
    _qlKind :: !QueryLogKind
  }

data QueryLogKind
  = QueryLogKindDatabase (Maybe (BackendResolvedConnectionTemplate))
  | QueryLogKindAction
  | QueryLogKindRemoteSchema
  | QueryLogKindCached
  | QueryLogKindIntrospection

instance J.ToJSON QueryLogKind where
  toJSON = \case
    QueryLogKindDatabase _ -> "database"
    QueryLogKindAction -> "action"
    QueryLogKindRemoteSchema -> "remote-schema"
    QueryLogKindCached -> "cached"
    QueryLogKindIntrospection -> "introspection"

data GeneratedQuery = GeneratedQuery
  { _gqQueryString :: Text,
    _gqPreparedArgs :: J.Value
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog gqlQuery generatedQuery reqId kind) =
    J.object
      $ [ "query" J..= gqlQuery,
          -- NOTE: this customizes the default JSON instance of a pair
          "generated_sql" J..= fmap fromPair generatedQuery,
          "request_id" J..= reqId,
          "kind" J..= kind
        ]
      <> maybe [] (\val -> ["connection_template" J..= val]) (getResolvedConnectionTemplate kind)
    where
      fromPair p = HashMap.fromList [first toTxt p]
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

class (Monad m) => MonadQueryLog m where
  logQueryLog ::
    L.Logger L.Hasura ->
    QueryLog ->
    m ()

instance (MonadQueryLog m) => MonadQueryLog (ExceptT e m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance (MonadQueryLog m) => MonadQueryLog (ReaderT r m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance (MonadQueryLog m) => MonadQueryLog (TraceT m) where
  logQueryLog logger l = lift $ logQueryLog logger l
