module Hasura.GraphQL.Logging
  ( logGraphqlQuery
  , mkQueryLog
  ) where

import qualified Data.Aeson                             as J
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import           Hasura.Prelude
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L

logGraphqlQuery
  :: (MonadIO m)
  => L.Logger
  -> L.VerboseLogging
  -> QueryLog
  -> m ()
logGraphqlQuery logger verbose =
  when (L.unVerboseLogging verbose) . liftIO . L.unLogger logger

data QueryLog
  = QueryLog
  { _qlQuery        :: !(Maybe GQLReqUnparsed)
  , _qlGeneratedSql :: !(Maybe EQ.GeneratedSql)
  , _qlRequestId    :: !RequestId
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog q sql reqId) =
    J.object [ "query" J..= q
             , "generated_sql" J..= (encodeSql <$> sql)
             , "request_id" J..= reqId
             ]

instance L.ToEngineLog QueryLog where
  toEngineLog ql = (L.LevelInfo, "query-log", J.toJSON ql)

encodeSql :: EQ.GeneratedSql -> J.Value
encodeSql sql =
  jValFromAssocList $
    map (\(a, q) -> (alName a, fmap J.toJSON q)) sql
  where
    alName = G.unName . G.unAlias
    jValFromAssocList xs = J.object $ map (uncurry (J..=)) xs

mkQueryLog :: RequestId -> GQLReqUnparsed -> Maybe EQ.GeneratedSql -> QueryLog
mkQueryLog reqId req sql = QueryLog (Just req) sql reqId
