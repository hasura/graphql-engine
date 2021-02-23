module Hasura.Backends.MSSQL.Connection where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.TH

import           Hasura.Incremental (Cacheable (..))

-- | ODBC connection string for MSSQL server
newtype MSSQLConnectionString
  = MSSQLConnectionString {unMSSQLConnectionString :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Cacheable, Hashable, NFData, Arbitrary)

data MSSQLConnectionInfo
  = MSSQLConnectionInfo
  { _mciConnectionString :: !MSSQLConnectionString
  } deriving (Show, Eq, Generic)
instance Cacheable MSSQLConnectionInfo
instance Hashable MSSQLConnectionInfo
instance NFData MSSQLConnectionInfo
instance Arbitrary MSSQLConnectionInfo where
  arbitrary = genericArbitrary
$(deriveJSON hasuraJSON ''MSSQLConnectionInfo)

data MSSQLConnConfiguration
  = MSSQLConnConfiguration
  { _mccConnectionInfo :: !MSSQLConnectionInfo
  } deriving (Show, Eq, Generic)
instance Cacheable MSSQLConnConfiguration
instance Hashable MSSQLConnConfiguration
instance NFData MSSQLConnConfiguration
$(deriveJSON hasuraJSON ''MSSQLConnConfiguration)

instance Arbitrary MSSQLConnConfiguration where
  arbitrary = genericArbitrary
