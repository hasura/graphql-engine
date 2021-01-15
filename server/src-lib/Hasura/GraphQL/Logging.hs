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
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.Server.Types                    (RequestId)
import           Hasura.Tracing                         (TraceT)

import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L


-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog
  = QueryLog
  { _qlQuery        :: !GQLReqUnparsed
  , _qlGeneratedSql :: !(Maybe (G.Name, EQ.PreparedSql))
  , _qlRequestId    :: !RequestId
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog q sql reqId) =
    J.object [ "query" J..= q
             , "generated_sql" J..= sql
             , "request_id" J..= reqId
             ]

instance L.ToEngineLog QueryLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)

class Monad m => MonadQueryLog m where
  logQueryLog
    :: L.Logger L.Hasura
    -- ^ logger
    -> GQLReqUnparsed
    -- ^ GraphQL request
    -> Maybe (G.Name, EQ.PreparedSql)
    -- ^ Generated SQL if any
    -> RequestId
    -- ^ unique identifier for a request. NOTE this can be spoofed!
    -> m ()

instance MonadQueryLog m => MonadQueryLog (ExceptT e m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId

instance MonadQueryLog m => MonadQueryLog (ReaderT r m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId

instance MonadQueryLog m => MonadQueryLog (TraceT m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId

instance MonadQueryLog m => MonadQueryLog (MetadataStorageT m) where
  logQueryLog l req sqlMap reqId = lift $ logQueryLog l req sqlMap reqId
