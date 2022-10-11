-- | MSSQL Connection Pooling
module Database.MSSQL.Pool
  ( -- * Types
    ConnectionString (..),
    ConnectionOptions (..),
    MSSQLPool (..),

    -- * Functions
    initMSSQLPool,
    drainMSSQLPool,
    withMSSQLPool,
  )
where

import Control.Exception.Lifted
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Pool qualified as Pool
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Prelude (Generic, Text)
import Prelude

-- | ODBC connection string for MSSQL server
newtype ConnectionString = ConnectionString {unConnectionString :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

data ConnectionOptions = ConnectionOptions
  { _coConnections :: Int,
    _coStripes :: Int,
    _coIdleTime :: Int
  }
  deriving (Show, Eq)

-- | ODBC connection pool
newtype MSSQLPool = MSSQLPool (Pool.Pool ODBC.Connection)

-- | Initialize an MSSQL pool with given connection configuration
initMSSQLPool ::
  ConnectionString ->
  ConnectionOptions ->
  IO MSSQLPool
initMSSQLPool (ConnectionString connString) ConnectionOptions {..} = do
  MSSQLPool
    <$> Pool.createPool
      (ODBC.connect connString)
      ODBC.close
      _coStripes
      (fromIntegral _coIdleTime)
      _coConnections

-- | Destroy all pool resources
drainMSSQLPool :: MSSQLPool -> IO ()
drainMSSQLPool (MSSQLPool pool) =
  Pool.destroyAllResources pool

withMSSQLPool ::
  (MonadBaseControl IO m) =>
  MSSQLPool ->
  (ODBC.Connection -> m a) ->
  m (Either ODBC.ODBCException a)
withMSSQLPool (MSSQLPool pool) action = do
  try $ Pool.withResource pool action
