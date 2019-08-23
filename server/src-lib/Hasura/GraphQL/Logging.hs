{-|
This module holds functions and data types used for logging at the GraphQL
layer. In contrast with, logging at the HTTP server layer.
-}

module Hasura.GraphQL.Logging
  ( logGraphqlQuery
  , QueryLog(..)
  ) where

import qualified Data.Aeson                             as J
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import           Hasura.Prelude
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L


-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog
  = QueryLog
  { _qlQuery        :: !GQLReqUnparsed
  , _qlGeneratedSql :: !(Maybe EQ.GeneratedSqlMap)
  , _qlRequestId    :: !RequestId
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog q sql reqId) =
    J.object [ "query" J..= q
             , "generated_sql" J..= (encodeSql <$> sql)
             , "request_id" J..= reqId
             ]

instance L.ToEngineLog QueryLog where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)

-- | Helper function to convert the list of alias to generated SQL into a
-- | key-value map to be printed as JSON
encodeSql :: EQ.GeneratedSqlMap -> J.Value
encodeSql sql =
  jValFromAssocList $ map (\(a, q) -> (alName a, fmap J.toJSON q)) sql
  where
    alName = G.unName . G.unAlias
    jValFromAssocList xs = J.object $ map (uncurry (J..=)) xs

{-|
Function to log a 'QueryLog'. This is meant to be used in execution of a
GraphQL query to log the GraphQL query and optionally the generated SQL.
-}
logGraphqlQuery
  :: (MonadIO m)
  => L.Logger
  -> QueryLog
  -> m ()
logGraphqlQuery logger = liftIO . L.unLogger logger
