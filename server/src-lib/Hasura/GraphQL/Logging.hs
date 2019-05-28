module Hasura.GraphQL.Logging
  ( logGraphqlQuery
  , mkQueryLog
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J

import           Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqUnparsed)
import           Hasura.Prelude

import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L

logGraphqlQuery :: (MonadIO m) => L.Logger -> QueryLog -> m ()
logGraphqlQuery logger = liftIO . L.unLogger logger

mkQueryLog :: GQLReqUnparsed -> Maybe EQ.GeneratedSql -> QueryLog
mkQueryLog req sql = QueryLog (Just req) (EQ.encodeSql <$> sql)

data QueryLog
  = QueryLog
  { _qlQuery        :: !(Maybe GQLReqUnparsed)
  , _qlGeneratedSql :: !(Maybe J.Value)
  }
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''QueryLog)

instance L.ToEngineLog QueryLog where
  toEngineLog ql = (L.LevelInfo, "query-log", J.toJSON ql)
