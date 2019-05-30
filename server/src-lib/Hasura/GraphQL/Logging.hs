module Hasura.GraphQL.Logging
  ( logGraphqlQuery
  , mkQueryLog
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J

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

mkQueryLog :: RequestId -> GQLReqUnparsed -> Maybe EQ.GeneratedSql
           -> QueryLog
mkQueryLog reqId req sql = QueryLog (Just req) (EQ.encodeSql <$> sql) reqId

data QueryLog
  = QueryLog
  { _qlQuery        :: !(Maybe GQLReqUnparsed)
  , _qlGeneratedSql :: !(Maybe J.Value)
  , _qlRequestId    :: !RequestId
  }
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''QueryLog)

instance L.ToEngineLog QueryLog where
  toEngineLog ql = (L.LevelInfo, "query-log", J.toJSON ql)
