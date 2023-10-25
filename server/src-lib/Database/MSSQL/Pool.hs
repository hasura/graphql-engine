-- | MSSQL Connection Pooling
module Database.MSSQL.Pool
  ( -- * Types
    ConnectionString (..),
    ConnectionOptions (..),
    MSSQLPool (..),
    PoolOptions (..),

    -- * Functions
    initMSSQLPool,
    drainMSSQLPool,
    withMSSQLPool,
    resizePool,
    getInUseConnections,
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
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

instance HasCodec ConnectionString where
  codec = dimapCodec ConnectionString unConnectionString codec

data ConnectionOptions
  = ConnectionOptionsPool PoolOptions
  | ConnectionOptionsNoPool
  deriving (Show, Eq)

data PoolOptions = PoolOptions
  { poConnections :: Int,
    poStripes :: Int,
    poIdleTime :: Int
  }
  deriving (Show, Eq)

-- | ODBC connection pool
data MSSQLPool
  = MSSQLPool (Pool.Pool ODBC.Connection)
  | MSSQLNoPool (IO ODBC.Connection)

-- | Initialize an MSSQL pool with given connection configuration
initMSSQLPool ::
  ConnectionString ->
  ConnectionOptions ->
  IO MSSQLPool
initMSSQLPool (ConnectionString connString) ConnectionOptionsNoPool = do
  return $ MSSQLNoPool (ODBC.connect connString)
initMSSQLPool (ConnectionString connString) (ConnectionOptionsPool PoolOptions {..}) = do
  MSSQLPool
    <$> Pool.createPool
      (ODBC.connect connString)
      ODBC.close
      poStripes
      (fromIntegral poIdleTime)
      poConnections

-- | Destroy all pool resources
drainMSSQLPool :: MSSQLPool -> IO ()
drainMSSQLPool (MSSQLPool pool) =
  Pool.destroyAllResources pool
drainMSSQLPool MSSQLNoPool {} = return ()

withMSSQLPool ::
  (MonadBaseControl IO m) =>
  MSSQLPool ->
  (ODBC.Connection -> m a) ->
  m (Either ODBC.ODBCException a)
withMSSQLPool (MSSQLPool pool) action = do
  try $ Pool.withResource pool action
withMSSQLPool (MSSQLNoPool connect) action = do
  try $ bracket (liftBaseWith (const connect)) (\conn -> liftBaseWith (const (ODBC.close conn))) action

-- | Resize a pool
resizePool :: MSSQLPool -> Int -> IO ()
resizePool (MSSQLPool pool) resizeTo = do
  -- Resize the pool max resources
  Pool.resizePool pool resizeTo
  -- Trim pool by destroying excess resources, if any
  Pool.tryTrimPool pool
resizePool (MSSQLNoPool {}) _ = return ()

getInUseConnections :: MSSQLPool -> IO Int
getInUseConnections (MSSQLPool pool) = Pool.getInUseResourceCount $ pool
getInUseConnections MSSQLNoPool {} = return 0
