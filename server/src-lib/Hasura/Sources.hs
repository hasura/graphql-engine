module Hasura.Sources where

import Hasura.Prelude
import Hasura.Incremental
import           Data.Aeson.TH

data DataSource
  = PostgresDB
  | MySQLDB
  deriving (Eq, Show, Generic)

instance Cacheable DataSource

$(deriveToJSON defaultOptions ''DataSource)
