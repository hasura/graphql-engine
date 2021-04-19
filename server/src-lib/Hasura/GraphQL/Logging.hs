{-|
This module holds functions and data types used for logging at the GraphQL
layer. In contrast with, logging at the HTTP server layer.
-}

module Hasura.GraphQL.Logging
  ( QueryLog(..)
  , GeneratedQuery(..)
  , MonadQueryLog(..)
  , QueryLogKind(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.Logging                         as L

import           Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import           Hasura.Metadata.Class
import           Hasura.Server.Types                    (RequestId)
import           Hasura.Tracing                         (TraceT)


-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog = QueryLog
  { _qlQuery        :: !GQLReqUnparsed
  , _qlGeneratedSql :: !(Maybe (G.Name, GeneratedQuery))
  , _qlRequestId    :: !RequestId
  , _qlKind         :: !QueryLogKind
  }

data QueryLogKind
  = Database
  | Action
  | RemoteSchema
  | GraphQL
  | Cached
  | Subscription -- This field is a workaround due to src-lib/Hasura/GraphQL/Transport/WebSocket.hs logging the query before execution is determined.

instance J.ToJSON QueryLogKind where
  toJSON = \case
    Database     -> "database"
    Action       -> "action"
    RemoteSchema -> "remote-schema"
    GraphQL      -> "graphql"
    Cached       -> "cached"
    Subscription -> "subscription"

data GeneratedQuery = GeneratedQuery
  { _gqQueryString  :: Text
  , _gqPreparedArgs :: J.Value
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog gqlQuery generatedQuery reqId kind) =
    J.object [ "query"         J..= gqlQuery
             , "generated_sql" J..= generatedQuery
             , "request_id"    J..= reqId
             , "kind"          J..= kind
             ]

instance J.ToJSON GeneratedQuery where
  toJSON (GeneratedQuery queryString preparedArgs) =
    J.object [ "query" J..= queryString
             , "prepared_arguments" J..= preparedArgs
             ]

instance L.ToEngineLog QueryLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)


class Monad m => MonadQueryLog m where
  logQueryLog
    :: L.Logger L.Hasura
    -> QueryLog
    -> m ()

instance MonadQueryLog m => MonadQueryLog (ExceptT e m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance MonadQueryLog m => MonadQueryLog (ReaderT r m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance MonadQueryLog m => MonadQueryLog (TraceT m) where
  logQueryLog logger l = lift $ logQueryLog logger l

instance MonadQueryLog m => MonadQueryLog (MetadataStorageT m) where
  logQueryLog logger l = lift $ logQueryLog logger l
