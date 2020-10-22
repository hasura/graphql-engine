module Hasura.RQL.Types.Source where

import           Hasura.Db
import           Hasura.Incremental         (Cacheable (..))
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Lift)

import qualified Database.PG.Query          as Q

data PGSourceConfig
  = PGSourceConfig
  { _pscExecCtx  :: !PGExecCtx
  , _pscConnInfo :: !Q.ConnInfo
  } deriving (Generic)

instance Eq PGSourceConfig where
  lconf == rconf = (_pscConnInfo lconf) == (_pscConnInfo rconf)

instance Cacheable PGSourceConfig where
  unchanged _ = (==)

instance ToJSON PGSourceConfig where
  toJSON = toJSON . show . _pscConnInfo

runPgSourceReadTx
  :: (MonadIO m) => PGSourceConfig -> Q.TxE QErr a -> m (Either QErr a)
runPgSourceReadTx psc =
  liftIO . runExceptT . (_pecRunReadNoTx $ _pscExecCtx psc)

runPgSourceWriteTx
  :: (MonadIO m) => PGSourceConfig -> Q.TxE QErr a -> m (Either QErr a)
runPgSourceWriteTx psc =
  liftIO . runExceptT . (_pecRunReadWrite $ _pscExecCtx psc)

data PGSourceSchemaCache
  = PGSourceSchemaCache
  { _pcTables        :: !TableCache
  , _pcFunctions     :: !FunctionCache
  , _pcConfiguration :: !PGSourceConfig
  }
$(deriveToJSON (aesonDrop 3 snakeCase) ''PGSourceSchemaCache)

type PGSourcesCache = HashMap SourceName PGSourceSchemaCache

data ResolvedSource
  = ResolvedSource
  { _rsConfig    :: !PGSourceConfig
  , _rsTables    :: !PostgresTablesMetadata
  , _rsFunctions :: !PostgresFunctionsMetadata
  , _rsPgScalars :: !(HashSet PGScalarType)
  } deriving (Eq)

type SourceTables = HashMap SourceName TableCache

-- Metadata API related types

defaultConnSettings :: SourceConnSettings
defaultConnSettings =
  SourceConnSettings
  { _scsMaxConnections = 50
  , _scsIdleTimeout    = 180
  , _scsRetries        = 1
  }

data SourceConnSettings
  = SourceConnSettings
  { _scsMaxConnections :: !Int
  , _scsIdleTimeout    :: !Int
  , _scsRetries        :: !Int
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable SourceConnSettings
$(deriveToJSON (aesonDrop 4 snakeCase) ''SourceConnSettings)

instance FromJSON SourceConnSettings where
  parseJSON = withObject "Object" $ \o ->
    SourceConnSettings
      <$> o .:? "max_connections" .!= _scsMaxConnections defaultConnSettings
      <*> o .:? "idle_timeout"    .!= _scsIdleTimeout    defaultConnSettings
      <*> o .:? "retries"         .!= _scsRetries        defaultConnSettings

data AddPgSource
  = AddPgSource
  { _apsName                   :: !SourceName
  , _apsDatabaseUrl            :: !UrlConf
  , _apsConnectionPoolSettings :: !SourceConnSettings
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 4 snakeCase) ''AddPgSource)

instance FromJSON AddPgSource where
  parseJSON = withObject "Object" $ \o ->
    AddPgSource
      <$> o .: "name"
      <*> o .: "database_url"
      <*> o .:? "connection_pool_settings" .!= defaultConnSettings

data DropPgSource
  = DropPgSource
  { _dpsName    :: !SourceName
  , _dpsCascade :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 4 snakeCase) ''DropPgSource)

instance FromJSON DropPgSource where
  parseJSON = withObject "Object" $ \o ->
    DropPgSource <$> o .: "name" <*> o .:? "cascade" .!= False

newtype PGSourceName =
  PGSourceName {_psnName :: SourceName}
  deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''PGSourceName)
