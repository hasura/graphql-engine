{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Connection
--
--   This module handles the connection against an MS SQL Server.
--   It defines the connection string, connection pool, default settings,
--   and conversion functions between MSSQL and graphql-engine.
module Hasura.Backends.MSSQL.Connection
  ( MSSQLConnConfiguration (MSSQLConnConfiguration),
    MSSQLSourceConfig (MSSQLSourceConfig, _mscExecCtx, _mscReadReplicas),
    MSSQLConnectionInfo (..),
    MSSQLPoolSettings (..),
    MSSQLExecCtx (..),
    MonadMSSQLTx (..),
    defaultMSSQLMaxConnections,
    createMSSQLPool,
    resizeMSSQLPool,
    getEnv,
    odbcValueToJValue,
    mkMSSQLExecCtx,
    mkMSSQLAnyQueryTx,
    runMSSQLSourceReadTx,
    runMSSQLSourceWriteTx,
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, disjointEitherCodec, optionalFieldOrNull', optionalFieldWithDefault', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (fromEnvCodec)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.Text (pack, unpack)
import Data.Time (localTimeToUTC)
import Database.MSSQL.Pool qualified as MSPool
import Database.MSSQL.Transaction qualified as MSTx
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.ResizePool

class MonadError QErr m => MonadMSSQLTx m where
  liftMSSQLTx :: MSTx.TxE QErr a -> m a

instance MonadMSSQLTx m => MonadMSSQLTx (ReaderT s m) where
  liftMSSQLTx = lift . liftMSSQLTx

instance MonadMSSQLTx m => MonadMSSQLTx (StateT s m) where
  liftMSSQLTx = lift . liftMSSQLTx

instance (Monoid w, MonadMSSQLTx m) => MonadMSSQLTx (WriterT w m) where
  liftMSSQLTx = lift . liftMSSQLTx

instance MonadIO m => MonadMSSQLTx (MSTx.TxET QErr m) where
  liftMSSQLTx = hoist liftIO

-- | ODBC connection string for MSSQL server
newtype MSSQLConnectionString = MSSQLConnectionString {unMSSQLConnectionString :: Text}
  deriving (Show, Eq, ToJSON, FromJSON, Hashable, NFData)

-- * Orphan instances

instance Hashable MSPool.ConnectionString

instance NFData MSPool.ConnectionString

data InputConnectionString
  = RawString MSPool.ConnectionString
  | FromEnvironment Text
  deriving stock (Show, Eq, Generic)

instance Hashable InputConnectionString

instance NFData InputConnectionString

instance HasCodec InputConnectionString where
  codec =
    dimapCodec
      (either RawString FromEnvironment)
      (\case RawString m -> Left m; FromEnvironment wEnv -> Right wEnv)
      $ disjointEitherCodec codec fromEnvCodec

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
  { _mpsMaxConnections :: Maybe Int,
    _mpsTotalMaxConnections :: Maybe Int,
    _mpsIdleTimeout :: Int
  }
  deriving (Show, Eq, Generic)

instance Hashable MSSQLPoolSettings

instance NFData MSSQLPoolSettings

instance ToJSON MSSQLPoolSettings where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance FromJSON MSSQLPoolSettings where
  parseJSON = withObject "MSSQL pool settings" $ \o ->
    MSSQLPoolSettings
      <$> o .:? "max_connections"
      <*> o .:? "total_max_connections"
      <*> o .:? "idle_timeout" .!= _mpsIdleTimeout defaultMSSQLPoolSettings

instance HasCodec MSSQLPoolSettings where
  codec =
    AC.object "MSSQLPoolSettings" $
      MSSQLPoolSettings
        <$> optionalFieldWithDefault' "max_connections" (Just defaultMSSQLMaxConnections) AC..= _mpsMaxConnections
        <*> optionalFieldOrNull' "total_max_connections" AC..= _mpsTotalMaxConnections
        <*> optionalFieldWithDefault' "idle_timeout" (_mpsIdleTimeout defaultMSSQLPoolSettings) AC..= _mpsIdleTimeout

defaultMSSQLMaxConnections :: Int
defaultMSSQLMaxConnections = 50

defaultMSSQLPoolSettings :: MSSQLPoolSettings
defaultMSSQLPoolSettings =
  MSSQLPoolSettings
    { _mpsMaxConnections = Nothing,
      _mpsTotalMaxConnections = Nothing,
      _mpsIdleTimeout = 5
    }

data MSSQLConnectionInfo = MSSQLConnectionInfo
  { _mciConnectionString :: InputConnectionString,
    _mciPoolSettings :: MSSQLPoolSettings
  }
  deriving (Show, Eq, Generic)

instance Hashable MSSQLConnectionInfo

instance NFData MSSQLConnectionInfo

instance HasCodec MSSQLConnectionInfo where
  codec =
    AC.object "MSSQLConnectionInfo" $
      MSSQLConnectionInfo
        <$> requiredField' "connection_string" AC..= _mciConnectionString
        <*> requiredField' "pool_settings" AC..= _mciPoolSettings

instance ToJSON MSSQLConnectionInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance FromJSON MSSQLConnectionInfo where
  parseJSON = withObject "Object" $ \o ->
    MSSQLConnectionInfo
      <$> ((o .: "database_url") <|> (o .: "connection_string"))
      <*> o .:? "pool_settings" .!= defaultMSSQLPoolSettings

data MSSQLConnConfiguration = MSSQLConnConfiguration
  { _mccConnectionInfo :: MSSQLConnectionInfo,
    _mccReadReplicas :: Maybe (NonEmpty MSSQLConnectionInfo)
  }
  deriving (Show, Eq, Generic)

instance Hashable MSSQLConnConfiguration

instance NFData MSSQLConnConfiguration

instance HasCodec MSSQLConnConfiguration where
  codec =
    AC.object "MSSQLConnConfiguration" $
      MSSQLConnConfiguration
        <$> requiredField' "connection_info" AC..= _mccConnectionInfo
        <*> optionalFieldOrNull' "read_replicas" AC..= _mccReadReplicas

instance FromJSON MSSQLConnConfiguration where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

instance ToJSON MSSQLConnConfiguration where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

createMSSQLPool ::
  MonadIO m =>
  QErrM m =>
  InputConnectionString ->
  MSPool.ConnectionOptions ->
  Env.Environment ->
  m (MSPool.ConnectionString, MSPool.MSSQLPool)
createMSSQLPool iConnString connOptions env = do
  connString <- resolveInputConnectionString env iConnString
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
    -- | A function that runs a transaction in the SERIALIZABLE transaction isolation
    --   level. This is mainly intended to run source catalog migrations.
    mssqlRunSerializableTx :: MSSQLRunTx,
    -- | Destroys connection pools
    mssqlDestroyConn :: IO (),
    -- | Resize pools based on number of server instances
    mssqlResizePools :: ServerReplicas -> IO SourceResizePoolSummary
  }

-- | Creates a MSSQL execution context for a single primary pool
mkMSSQLExecCtx :: MSPool.MSSQLPool -> ResizePoolStrategy -> MSSQLExecCtx
mkMSSQLExecCtx pool resizeStrategy =
  MSSQLExecCtx
    { mssqlRunReadOnly = \tx -> MSTx.runTxE defaultMSSQLTxErrorHandler MSTx.ReadCommitted tx pool,
      mssqlRunReadWrite = \tx -> MSTx.runTxE defaultMSSQLTxErrorHandler MSTx.ReadCommitted tx pool,
      mssqlRunSerializableTx = \tx -> MSTx.runTxE defaultMSSQLTxErrorHandler MSTx.Serializable tx pool,
      mssqlDestroyConn = MSPool.drainMSSQLPool pool,
      mssqlResizePools =
        case resizeStrategy of
          NeverResizePool -> const $ pure noPoolsResizedSummary
          ResizePool maxConnections -> resizeMSSQLPool' maxConnections
    }
  where
    resizeMSSQLPool' maxConnections serverReplicas = do
      -- Resize the primary pool
      resizeMSSQLPool pool maxConnections serverReplicas
      -- Return the summary. Only the primary pool is resized
      pure $
        SourceResizePoolSummary
          { _srpsPrimaryResized = True,
            _srpsReadReplicasResized = False,
            _srpsConnectionSet = []
          }

-- | Resize MSSQL pool by setting the number of connections equal to
-- allowed maximum connections across all server instances divided by
-- number of instances
resizeMSSQLPool :: MSPool.MSSQLPool -> Int -> ServerReplicas -> IO ()
resizeMSSQLPool mssqlPool maxConnections serverReplicas =
  MSPool.resizePool mssqlPool (maxConnections `div` getServerReplicasInt serverReplicas)

-- | Run any query discarding its results
mkMSSQLAnyQueryTx :: ODBC.Query -> MSTx.TxET QErr IO ()
mkMSSQLAnyQueryTx q = do
  _discard :: [[ODBC.Value]] <- MSTx.multiRowQueryE defaultMSSQLTxErrorHandler q
  pure ()

data MSSQLSourceConfig = MSSQLSourceConfig
  { _mscConnectionString :: MSPool.ConnectionString,
    _mscExecCtx :: MSSQLExecCtx,
    -- | Number of read replicas used by the execution context
    _mscReadReplicas :: Int
  }
  deriving (Generic)

instance Show MSSQLSourceConfig where
  show = show . _mscConnectionString

instance Eq MSSQLSourceConfig where
  MSSQLSourceConfig connStr1 _ _ == MSSQLSourceConfig connStr2 _ _ =
    connStr1 == connStr2

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
  ODBC.ZonedTimeValue lt tz -> J.toJSON (localTimeToUTC tz lt)

runMSSQLSourceReadTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  MSTx.TxET QErr m a ->
  m (Either QErr a)
runMSSQLSourceReadTx msc =
  runExceptT . mssqlRunReadOnly (_mscExecCtx msc)

runMSSQLSourceWriteTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  MSSQLSourceConfig ->
  MSTx.TxET QErr m a ->
  m (Either QErr a)
runMSSQLSourceWriteTx msc =
  runExceptT . mssqlRunReadWrite (_mscExecCtx msc)
