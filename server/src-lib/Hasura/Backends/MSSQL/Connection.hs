module Hasura.Backends.MSSQL.Connection where

import           Hasura.Prelude

import qualified Data.Pool               as Pool
import qualified Database.ODBC.SQLServer as ODBC

import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment        as Env
import           Data.Text               (pack, unpack)
import           Hasura.Base.Error
import           Hasura.Incremental      (Cacheable (..))

-- | ODBC connection string for MSSQL server
newtype MSSQLConnectionString
  = MSSQLConnectionString {unMSSQLConnectionString :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Cacheable, Hashable, NFData, Arbitrary)

data InputConnectionString
  = RawString !MSSQLConnectionString
  | FromEnvironment !Text
  deriving stock (Show, Eq, Generic)
instance Cacheable InputConnectionString
instance Hashable InputConnectionString
instance NFData InputConnectionString

instance Arbitrary InputConnectionString where
  arbitrary = genericArbitrary

instance ToJSON InputConnectionString where
  toJSON =
    \case
      (RawString m)          -> toJSON m
      (FromEnvironment wEnv) -> object ["from_env" .= wEnv]

instance FromJSON InputConnectionString where
  parseJSON =
    \case
      (Object o)   -> FromEnvironment <$> o .: "from_env"
      s@(String _) -> RawString <$> parseJSON s
      _            -> fail "one of string or object must be provided"

data MSSQLPoolSettings
  = MSSQLPoolSettings
  { _mpsMaxConnections :: !Int
  , _mpsIdleTimeout    :: !Int
  } deriving (Show, Eq, Generic)
instance Cacheable MSSQLPoolSettings
instance Hashable MSSQLPoolSettings
instance NFData MSSQLPoolSettings
$(deriveToJSON hasuraJSON ''MSSQLPoolSettings)

instance FromJSON MSSQLPoolSettings where
  parseJSON = withObject "MSSQL pool settings" $ \o ->
    MSSQLPoolSettings
      <$> o .:? "max_connections" .!= _mpsMaxConnections defaultMSSQLPoolSettings
      <*> o .:? "idle_timeout"    .!= _mpsIdleTimeout    defaultMSSQLPoolSettings

instance Arbitrary MSSQLPoolSettings where
  arbitrary = genericArbitrary

defaultMSSQLPoolSettings :: MSSQLPoolSettings
defaultMSSQLPoolSettings =
  MSSQLPoolSettings
  { _mpsMaxConnections = 50
  , _mpsIdleTimeout    = 5
  }

data MSSQLConnectionInfo
  = MSSQLConnectionInfo
  { _mciConnectionString :: !InputConnectionString
  , _mciPoolSettings     :: !MSSQLPoolSettings
  } deriving (Show, Eq, Generic)
instance Cacheable MSSQLConnectionInfo
instance Hashable MSSQLConnectionInfo
instance NFData MSSQLConnectionInfo
$(deriveToJSON hasuraJSON ''MSSQLConnectionInfo)

instance Arbitrary MSSQLConnectionInfo where
  arbitrary = genericArbitrary


instance FromJSON MSSQLConnectionInfo where
  parseJSON = withObject "Object" $ \o ->
    MSSQLConnectionInfo
      <$> ((o .: "database_url") <|> (o .: "connection_string"))
      <*> o .:? "pool_settings" .!= defaultMSSQLPoolSettings

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

newtype MSSQLPool
  = MSSQLPool { unMSSQLPool :: Pool.Pool ODBC.Connection }

createMSSQLPool
  :: MonadIO m
  => QErrM m
  => MSSQLConnectionInfo
  -> m (MSSQLConnectionString, MSSQLPool)
createMSSQLPool (MSSQLConnectionInfo iConnString MSSQLPoolSettings{..}) = do
  env        <- liftIO Env.getEnvironment
  connString <- resolveInputConnectionString env iConnString
  pool       <- liftIO
                  $ MSSQLPool
                  <$> Pool.createPool
                        (ODBC.connect $ unMSSQLConnectionString connString)
                        ODBC.close
                        1
                        (fromIntegral _mpsIdleTimeout) _mpsMaxConnections
  pure (connString, pool)

resolveInputConnectionString
  :: QErrM m
  => Env.Environment
  -> InputConnectionString
  -> m MSSQLConnectionString
resolveInputConnectionString env =
  \case
      (RawString cs)           -> pure cs
      (FromEnvironment envVar) -> MSSQLConnectionString <$> getEnv env envVar

getEnv :: QErrM m => Env.Environment -> Text -> m Text
getEnv env k = do
  let mEnv = Env.lookupEnv env (unpack k)
  case mEnv of
    Nothing     -> throw400 NotFound $ "environment variable '" <> k <> "' not set"
    Just envVal -> return (pack envVal)

drainMSSQLPool :: MSSQLPool -> IO ()
drainMSSQLPool (MSSQLPool pool) =
  Pool.destroyAllResources pool

odbcExceptionToJSONValue :: ODBC.ODBCException -> Value
odbcExceptionToJSONValue =
  $(mkToJSON defaultOptions{constructorTagModifier = snakeCase} ''ODBC.ODBCException)

runJSONPathQuery
  :: (MonadError QErr m, MonadIO m)
  => MSSQLPool -> ODBC.Query -> m Text
runJSONPathQuery pool query =
  mconcat <$> withMSSQLPool pool (`ODBC.query` query)

withMSSQLPool
  :: (MonadError QErr m, MonadIO m)
  => MSSQLPool -> (ODBC.Connection -> IO a) -> m a
withMSSQLPool (MSSQLPool pool) f = do
  res <- liftIO $ try $ Pool.withResource pool f
  onLeft res $ \e ->
    throw500WithDetail "sql server exception" $ odbcExceptionToJSONValue e

data MSSQLSourceConfig
  = MSSQLSourceConfig
  { _mscConnectionString :: !MSSQLConnectionString
  , _mscConnectionPool   :: !MSSQLPool
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
