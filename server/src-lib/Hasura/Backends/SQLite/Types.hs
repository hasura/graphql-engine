module Hasura.Backends.SQLite.Types where

import           Hasura.Prelude

import qualified Database.SQLite.Simple as L

import           Data.Aeson

import           Hasura.Incremental     (Cacheable (..))


newtype SLFilePath = SLFilePath String
  deriving newtype (Show, Eq, Hashable, Cacheable, NFData, ToJSON, FromJSON, Arbitrary)

data SLSourceConfig = SLSourceConfig -- TODO

instance Show SLSourceConfig where
  show = undefined -- TODO

instance Eq SLSourceConfig where
  (==) = undefined -- TODO

instance Cacheable SLSourceConfig where
  unchanged _ = undefined -- TODO

instance ToJSON SLSourceConfig where
  toJSON = undefined -- TODO
