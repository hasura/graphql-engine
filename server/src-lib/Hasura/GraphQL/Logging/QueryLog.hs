-- |
-- This module holds functions and data types used for logging at the GraphQL
-- layer. In contrast with, logging at the HTTP server layer.
module Hasura.GraphQL.Logging.QueryLog
  ( QueryLog (..),
    GeneratedQuery (..),
    MonadQueryLog (..),
    QueryLogKind (..),
    maskQueryLog,
    maskJsonValue,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text.Extended
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.ConnectionTemplate (BackendResolvedConnectionTemplate (..))
import Hasura.Server.Types (RequestId)
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G

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

-- | Mask sensitive variable values in a 'QueryLog' before it is emitted.
-- If the set of keys to mask is empty, the original 'QueryLog' is returned
-- unchanged in O(1) time.
maskQueryLog :: HashSet Text -> QueryLog -> QueryLog
maskQueryLog maskedKeys ql
  | HashSet.null maskedKeys = ql
  | otherwise =
      ql
        { _qlQuery = maskGQLReqVariables maskedKeys (_qlQuery ql),
          _qlGeneratedSql = fmap (fmap (maskGeneratedQuery maskedKeys)) (_qlGeneratedSql ql)
        }

-- | Mask variable values in the GraphQL request whose names match the
-- configured set of masked variable names.
maskGQLReqVariables :: HashSet Text -> GQLReqUnparsed -> GQLReqUnparsed
maskGQLReqVariables maskedKeys (Protocol.GQLReq opName query vars) =
  Protocol.GQLReq opName query (fmap (maskVariableValues maskedKeys) vars)

-- | Replace values in a 'VariableValues' map with "[MASKED]" when their
-- key name is in the masked set.
maskVariableValues :: HashSet Text -> Protocol.VariableValues -> Protocol.VariableValues
maskVariableValues maskedKeys = HashMap.mapWithKey maskIfSensitive
  where
    maskIfSensitive :: G.Name -> J.Value -> J.Value
    maskIfSensitive name val
      | G.unName name `HashSet.member` maskedKeys = J.String "[MASKED]"
      | otherwise = maskJsonValue maskedKeys val

-- | Mask sensitive keys in a 'GeneratedQuery''s prepared arguments.
maskGeneratedQuery :: HashSet Text -> GeneratedQuery -> GeneratedQuery
maskGeneratedQuery maskedKeys gq =
  gq {_gqPreparedArgs = maskJsonValue maskedKeys (_gqPreparedArgs gq)}

-- | Recursively traverse a JSON 'Value', replacing the values of any object
-- keys that match the masked set with "[MASKED]".
maskJsonValue :: HashSet Text -> J.Value -> J.Value
maskJsonValue maskedKeys = go
  where
    go (J.Object obj) = J.Object $ KM.mapWithKey maskKey obj
    go (J.Array arr) = J.Array $ fmap go arr
    go other = other

    maskKey :: K.Key -> J.Value -> J.Value
    maskKey k v
      | K.toText k `HashSet.member` maskedKeys = J.String "[MASKED]"
      | otherwise = go v
