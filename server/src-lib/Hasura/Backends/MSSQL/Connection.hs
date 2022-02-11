{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Connection
--
--   This module handles the connection against an MS SQL Server.
--   It defines the connection string, connection pool, default settings,
--   and conversion functions between MSSQL and graphql-engine.
module Hasura.Backends.MSSQL.Connection
  ( MSSQLConnConfiguration (MSSQLConnConfiguration),
    MSSQLSourceConfig (MSSQLSourceConfig, _mscExecCtx),
    MSSQLExecCtx (..),
    createMSSQLPool,
    getEnv,
    odbcValueToJValue,
    mkMSSQLExecCtx,
  )
where

import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.TH
import Data.Environment qualified as Env
import Data.Text (pack, unpack)
import Database.MSSQL.Pool qualified as MSPool
import Database.MSSQL.Transaction qualified as MSTx
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Base.Error
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude

-- * Orphan instances

instance Cacheable MSPool.ConnectionString

instance Hashable MSPool.ConnectionString

instance NFData MSPool.ConnectionString

data InputConnectionString
  = RawString !MSPool.ConnectionString
  | FromEnvironment !Text
  deriving stock (Show, Eq, Generic)

instance Cacheable InputConnectionString

instance Hashable InputConnectionString

instance NFData InputConnectionString

instance ToJSON InputConnectionString where
  toJSON =
    \case
      (RawString m) -> toJSON m
      (FromEnvironment wEnv) -> object ["from_env" .= wEnv]

instance FromJSON InputConnectionString where
  parseJSON =
    \case
      (Object o) -> FromEnvironment <$> o .: "from_env"
      s@(String _) -> RawString <$> parseJSON s
      _ -> fail "one of string or object must be provided"

data MSSQLPoolSettings = MSSQLPoolSettings
  { _mpsMaxConnections :: !Int,
    _mpsIdleTimeout :: !Int
  }
  deriving (Show, Eq, Generic)

instance Cacheable MSSQLPoolSettings

instance Hashable MSSQLPoolSettings

instance NFData MSSQLPoolSettings

$(deriveToJSON hasuraJSON ''MSSQLPoolSettings)

instance FromJSON MSSQLPoolSettings where
  parseJSON = withObject "MSSQL pool settings" $ \o ->
    MSSQLPoolSettings
      <$> o .:? "max_connections" .!= _mpsMaxConnections defaultMSSQLPoolSettings
      <*> o .:? "idle_timeout" .!= _mpsIdleTimeout defaultMSSQLPoolSettings

defaultMSSQLPoolSettings :: MSSQLPoolSettings
defaultMSSQLPoolSettings =
  MSSQLPoolSettings
    { _mpsMaxConnections = 50,
      _mpsIdleTimeout = 5
    }

data MSSQLConnectionInfo = MSSQLConnectionInfo
  { _mciConnectionString :: !InputConnectionString,
    _mciPoolSettings :: !MSSQLPoolSettings
  }
  deriving (Show, Eq, Generic)

instance Cacheable MSSQLConnectionInfo

instance Hashable MSSQLConnectionInfo

instance NFData MSSQLConnectionInfo

$(deriveToJSON hasuraJSON ''MSSQLConnectionInfo)

instance FromJSON MSSQLConnectionInfo where
  parseJSON = withObject "Object" $ \o ->
    MSSQLConnectionInfo
      <$> ((o .: "database_url") <|> (o .: "connection_string"))
      <*> o .:? "pool_settings" .!= defaultMSSQLPoolSettings

data MSSQLConnConfiguration = MSSQLConnConfiguration
  { _mccConnectionInfo :: !MSSQLConnectionInfo,
    _mccReadReplicas :: !(Maybe (NonEmpty MSSQLConnectionInfo))
  }
  deriving (Show, Eq, Generic)

instance Cacheable MSSQLConnConfiguration

instance Hashable MSSQLConnConfiguration

instance NFData MSSQLConnConfiguration

$(deriveJSON hasuraJSON {omitNothingFields = True} ''MSSQLConnConfiguration)

createMSSQLPool ::
  MonadIO m =>
  QErrM m =>
  MSSQLConnectionInfo ->
  Env.Environment ->
  m (MSPool.ConnectionString, MSPool.MSSQLPool)
createMSSQLPool (MSSQLConnectionInfo iConnString MSSQLPoolSettings {..}) env = do
  connString <- resolveInputConnectionString env iConnString
  let connOptions =
        MSPool.ConnectionOptions
          { _coConnections = _mpsMaxConnections,
            _coStripes = 1,
            _coIdleTime = fromIntegral _mpsIdleTimeout
          }
  pool <- liftIO $ MSPool.initMSSQLPool connString connOptions
  pure (connString, pool)

resolveInputConnectionString ::
  QErrM m =>
  Env.Environment ->
  InputConnectionString ->
  m MSPool.ConnectionString
resolveInputConnectionString env =
  \case
    (RawString cs) -> pure cs
    (FromEnvironment envVar) -> MSPool.ConnectionString <$> getEnv env envVar

getEnv :: QErrM m => Env.Environment -> Text -> m Text
getEnv env k = do
  let mEnv = Env.lookupEnv env (unpack k)
  case mEnv of
    Nothing -> throw400 NotFound $ "environment variable '" <> k <> "' not set"
    Just envVal -> return (pack envVal)

type MSSQLRunTx =
  forall m a. (MonadIO m, MonadBaseControl IO m) => MSTx.TxET QErr m a -> ExceptT QErr m a

-- | Execution Context required to execute MSSQL transactions
data MSSQLExecCtx = MSSQLExecCtx
  { -- | A function that runs read-only queries
    mssqlRunReadOnly :: MSSQLRunTx,
    -- | A function that runs read-write queries; run in a transaction
    mssqlRunReadWrite :: MSSQLRunTx,
    -- | Destroys connection pools
    mssqlDestroyConn :: IO ()
  }

-- | Creates a MSSQL execution context for a single primary pool
mkMSSQLExecCtx :: MSPool.MSSQLPool -> MSSQLExecCtx
mkMSSQLExecCtx pool =
  MSSQLExecCtx
    { mssqlRunReadOnly = \tx -> MSTx.runTxE defaultMSSQLTxErrorHandler tx pool,
      mssqlRunReadWrite = \tx -> MSTx.runTxE defaultMSSQLTxErrorHandler tx pool,
      mssqlDestroyConn = MSPool.drainMSSQLPool pool
    }

data MSSQLSourceConfig = MSSQLSourceConfig
  { _mscConnectionString :: !MSPool.ConnectionString,
    _mscExecCtx :: !MSSQLExecCtx
  }
  deriving (Generic)

instance Show MSSQLSourceConfig where
  show = show . _mscConnectionString

instance Eq MSSQLSourceConfig where
  MSSQLSourceConfig connStr1 _ == MSSQLSourceConfig connStr2 _ =
    connStr1 == connStr2

instance Cacheable MSSQLSourceConfig where
  unchanged _ = (==)

instance ToJSON MSSQLSourceConfig where
  toJSON = toJSON . _mscConnectionString

odbcValueToJValue :: ODBC.Value -> J.Value
odbcValueToJValue = \case
  ODBC.TextValue t -> J.String t
  ODBC.ByteStringValue b -> J.String $ bsToTxt b
  ODBC.BinaryValue b -> J.String $ bsToTxt $ ODBC.unBinary b
  ODBC.BoolValue b -> J.Bool b
  ODBC.DoubleValue d -> J.toJSON d
  ODBC.FloatValue f -> J.toJSON f
  ODBC.IntValue i -> J.toJSON i
  ODBC.ByteValue b -> J.toJSON b
  ODBC.DayValue d -> J.toJSON d
  ODBC.TimeOfDayValue td -> J.toJSON td
  ODBC.LocalTimeValue l -> J.toJSON l
  ODBC.NullValue -> J.Null
