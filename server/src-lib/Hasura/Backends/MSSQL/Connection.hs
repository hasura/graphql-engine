-- | MSSQL Connection
--
--   This module handles the connection against an MS SQL Server.
--   It defines the connection string, connection pool, default settings,
--   and conversion functions between MSSQL and graphql-engine.
module Hasura.Backends.MSSQL.Connection
  ( MSSQLConnConfiguration (MSSQLConnConfiguration),
    MSSQLPool,
    MSSQLSourceConfig (MSSQLSourceConfig, _mscExecCtx),
    MSSQLExecCtx (..),
    createMSSQLPool,
    drainMSSQLPool,
    fromMSSQLTxError,
    getEnv,
    odbcExceptionToJSONValue,
    odbcValueToJValue,
    runJSONPathQuery,
    withMSSQLPool,
    mkMSSQLExecCtx,
  )
where

import Control.Exception.Lifted qualified as EL
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Environment qualified as Env
import Data.Pool qualified as Pool
import Data.Text (pack, unpack)
import Database.MSSQL.Transaction
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Base.Error
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude

-- | ODBC connection string for MSSQL server
newtype MSSQLConnectionString = MSSQLConnectionString {unMSSQLConnectionString :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Cacheable, Hashable, NFData)

data InputConnectionString
  = RawString !MSSQLConnectionString
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

newtype MSSQLPool = MSSQLPool (Pool.Pool ODBC.Connection)

createMSSQLPool ::
  MonadIO m =>
  QErrM m =>
  MSSQLConnectionInfo ->
  Env.Environment ->
  m (MSSQLConnectionString, MSSQLPool)
createMSSQLPool (MSSQLConnectionInfo iConnString MSSQLPoolSettings {..}) env = do
  connString <- resolveInputConnectionString env iConnString
  pool <-
    liftIO $
      MSSQLPool
        <$> Pool.createPool
          (ODBC.connect $ unMSSQLConnectionString connString)
          ODBC.close
          1
          (fromIntegral _mpsIdleTimeout)
          _mpsMaxConnections
  pure (connString, pool)

resolveInputConnectionString ::
  QErrM m =>
  Env.Environment ->
  InputConnectionString ->
  m MSSQLConnectionString
resolveInputConnectionString env =
  \case
    (RawString cs) -> pure cs
    (FromEnvironment envVar) -> MSSQLConnectionString <$> getEnv env envVar

getEnv :: QErrM m => Env.Environment -> Text -> m Text
getEnv env k = do
  let mEnv = Env.lookupEnv env (unpack k)
  case mEnv of
    Nothing -> throw400 NotFound $ "environment variable '" <> k <> "' not set"
    Just envVal -> return (pack envVal)

drainMSSQLPool :: MSSQLPool -> IO ()
drainMSSQLPool (MSSQLPool pool) =
  Pool.destroyAllResources pool

odbcExceptionToJSONValue :: ODBC.ODBCException -> Value
odbcExceptionToJSONValue =
  $(mkToJSON defaultOptions {constructorTagModifier = snakeCase} ''ODBC.ODBCException)

runJSONPathQuery ::
  (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
  MSSQLRunTx ->
  ODBC.Query ->
  m Text
runJSONPathQuery mssqlRunTx query = mconcat <$> mssqlRunTx (`ODBC.query` query)

withMSSQLPool ::
  (MonadIO m, MonadBaseControl IO m, MonadError QErr m) =>
  MSSQLPool ->
  (ODBC.Connection -> m a) ->
  m a
withMSSQLPool (MSSQLPool pool) f = do
  res <- EL.try $ Pool.withResource pool f
  onLeft res $ \e ->
    throw500WithDetail "sql server exception" $ odbcExceptionToJSONValue e

type MSSQLRunTx =
  forall m a. (MonadError QErr m, MonadIO m, MonadBaseControl IO m) => (ODBC.Connection -> m a) -> m a

-- | Execution Context required to execute MSSQL transactions
data MSSQLExecCtx = MSSQLExecCtx
  { -- | A function that runs read-only queries
    mssqlRunReadOnly :: !MSSQLRunTx,
    -- | A function that runs read-write queries
    mssqlRunReadWrite :: !MSSQLRunTx,
    -- | Destroys connection pools
    mssqlDestroyConn :: IO ()
  }

-- | Creates a MSSQL execution context for a single primary pool
mkMSSQLExecCtx :: MSSQLPool -> MSSQLExecCtx
mkMSSQLExecCtx pool =
  MSSQLExecCtx
    { mssqlRunReadOnly = withMSSQLPool pool,
      mssqlRunReadWrite = withMSSQLPool pool,
      mssqlDestroyConn = drainMSSQLPool pool
    }

data MSSQLSourceConfig = MSSQLSourceConfig
  { _mscConnectionString :: !MSSQLConnectionString,
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

newtype MSSQLConnErr = MSSQLConnErr {getConnErr :: Text}
  deriving (Show, Eq, ToJSON)

fromMSSQLTxError :: MSSQLTxError -> QErr
fromMSSQLTxError = \case
  MSSQLQueryError query exception ->
    (internalError "database query error")
      { qeInternal =
          Just $
            ExtraInternal $
              object
                [ "query" .= ODBC.renderQuery query,
                  "exception" .= odbcExceptionToJSONValue exception
                ]
      }
  MSSQLInternal err ->
    (internalError "mssql internal error")
      { qeInternal = Just $ ExtraInternal $ object ["error" .= err]
      }
