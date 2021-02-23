-- | Types for Transact-SQL aka T-SQL; the language of SQL Server.

module Hasura.Backends.MSSQL.Types
  ( MSSQLSourceConfig(..)
  , MSSQLRunSQL(..)
  , module Hasura.Backends.MSSQL.Types.Internal
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.TH

import qualified Database.ODBC.SQLServer               as ODBC

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Types.Instances ()
import           Hasura.Backends.MSSQL.Types.Internal
import           Hasura.Incremental                    (Cacheable (..))
import           Hasura.RQL.Types.Common

data MSSQLSourceConfig
  = MSSQLSourceConfig
  { _mscConnectionString :: !MSSQLConnectionString
  , _mscConnection       :: !ODBC.Connection
  } deriving (Generic)

instance Show MSSQLSourceConfig where
  show = show . _mscConnectionString

instance Eq MSSQLSourceConfig where
  MSSQLSourceConfig connStr1 _ == MSSQLSourceConfig connStr2 _ =
    connStr1 == connStr2

instance Cacheable MSSQLSourceConfig where
  unchanged _ = (==)

instance ToJSON MSSQLSourceConfig where
  toJSON = toJSON . _mscConnectionString

data MSSQLRunSQL
  = MSSQLRunSQL
  { _mrsSql    :: Text
  , _mrsSource :: !SourceName
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''MSSQLRunSQL)
