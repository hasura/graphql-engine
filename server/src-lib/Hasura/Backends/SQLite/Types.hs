module Hasura.Backends.SQLite.Types where

import           Hasura.Prelude

import qualified Database.SQLite.Simple as L

import           Data.Aeson

import           Hasura.Incremental     (Cacheable (..))


newtype SLFilePath = SLFilePath String
  deriving newtype (Show, Eq, Hashable, Cacheable, NFData, ToJSON, FromJSON, Arbitrary)

data SLSourceConfig = SLSourceConfig
  { slPath :: SLFilePath
  , slConn :: L.Connection
  } deriving Generic

instance Show SLSourceConfig where
  show = show . slPath

instance Eq SLSourceConfig where
  (==) = (==) `on` slPath

instance Cacheable SLSourceConfig where
  unchanged _ = (==)

instance ToJSON SLSourceConfig where
  toJSON = toJSON . slPath
