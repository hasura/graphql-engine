{-|
This module holds functions and data types used for logging at the GraphQL
layer. In contrast with, logging at the HTTP server layer.
-}

module Hasura.GraphQL.Logging
  ( QueryLog(..)
  , MonadQueryLog(..)
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

instance L.ToEngineLog QueryLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)

-- | Helper function to convert the list of alias to generated SQL into a
-- | key-value map to be printed as JSON
encodeSql :: EQ.GeneratedSqlMap -> J.Value
encodeSql sql =
  jValFromAssocList $ map (\(a, q) -> (G.unName a, fmap J.toJSON q)) sql
  where
    jValFromAssocList xs = J.object $ map (uncurry (J..=)) xs

class Monad m => MonadQueryLog m where
  logQueryLog
    :: L.Logger L.Hasura
    -- ^ logger
    -> GQLReqUnparsed
    -- ^ GraphQL request
    -> (Maybe EQ.GeneratedSqlMap)
    -- ^ Generated SQL if any
    -> RequestId
    -> m ()

instance MonadQueryLog m => MonadQueryLog (ExceptT e m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId

instance MonadQueryLog m => MonadQueryLog (ReaderT r m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId
