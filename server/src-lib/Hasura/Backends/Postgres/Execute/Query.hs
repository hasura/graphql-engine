module Hasura.Backends.Postgres.Execute.Query where

import           Hasura.Prelude

import qualified Data.Aeson                               as J
import qualified Database.PG.Query                        as Q


import           Hasura.Backends.Postgres.Execute.Prepare
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.RQL.DML.RemoteJoin.Types
import           Hasura.SQL.Backend

data PreparedSql = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: !PrepArgMap
  , _psRemoteJoins :: !(Maybe (RemoteJoins 'Postgres))
  }

instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs _) =
    J.object [ "query" J..= Q.getQueryText q
             , "prepared_arguments" J..= fmap (pgScalarValueToJson . snd) prepArgs
             ]
