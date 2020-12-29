module Hasura.RQL.Types.Source where

import           Hasura.Backends.Postgres.Connection
import           Hasura.Incremental                  (Cacheable (..))
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend

import qualified Hasura.Tracing                      as Tracing

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

data SourceInfo b
  = SourceInfo
  { _pcName          :: !SourceName
  , _pcTables        :: !(TableCache b)
  , _pcFunctions     :: !FunctionCache
  , _pcConfiguration :: !(SourceConfig b)
  } deriving (Generic)
$(makeLenses ''SourceInfo)
instance ToJSON (SourceInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

type SourceCache b = HashMap SourceName (SourceInfo b)

-- | Contains Postgres connection configuration and essential metadata from the
-- database to build schema cache for tables and function.
data ResolvedPGSource
  = ResolvedPGSource
  { _rsConfig    :: !(SourceConfig 'Postgres)
  , _rsTables    :: !(DBTablesMetadata 'Postgres)
  , _rsFunctions :: !PostgresFunctionsMetadata
  , _rsPgScalars :: !(HashSet (ScalarType 'Postgres))
  } deriving (Eq)

type SourceTables b = HashMap SourceName (TableCache b)

data PostgresPoolSettings
  = PostgresPoolSettings
  { _ppsMaxConnections :: !Int
  , _ppsIdleTimeout    :: !Int
  , _ppsRetries        :: !Int
  } deriving (Show, Eq, Generic)
instance Cacheable PostgresPoolSettings
$(deriveToJSON (aesonDrop 4 snakeCase) ''PostgresPoolSettings)

instance FromJSON PostgresPoolSettings where
  parseJSON = withObject "Object" $ \o ->
    PostgresPoolSettings
      <$> o .:? "max_connections" .!= _ppsMaxConnections defaultPostgresPoolSettings
      <*> o .:? "idle_timeout"    .!= _ppsIdleTimeout    defaultPostgresPoolSettings
      <*> o .:? "retries"         .!= _ppsRetries        defaultPostgresPoolSettings

defaultPostgresPoolSettings :: PostgresPoolSettings
defaultPostgresPoolSettings =
  PostgresPoolSettings
  { _ppsMaxConnections = 50
  , _ppsIdleTimeout    = 180
  , _ppsRetries        = 1
  }

data PostgresSourceConnInfo
  = PostgresSourceConnInfo
  { _psciDatabaseUrl  :: !UrlConf
  , _psciPoolSettings :: !PostgresPoolSettings
  } deriving (Show, Eq, Generic)
instance Cacheable PostgresSourceConnInfo
$(deriveJSON (aesonDrop 5 snakeCase) ''PostgresSourceConnInfo)

data SourceConfiguration
  = SourceConfiguration
  { _scConnectionInfo :: !PostgresSourceConnInfo
  , _scReadReplicas   :: !(Maybe (NonEmpty PostgresSourceConnInfo))
  } deriving (Show, Eq, Generic)
instance Cacheable SourceConfiguration
$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields = True} ''SourceConfiguration)

type SourceResolver =
  SourceConfiguration -> IO (Either QErr (SourceConfig 'Postgres))

class (Monad m) => MonadResolveSource m where
  getSourceResolver :: m SourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ExceptT e m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ReaderT r m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Tracing.TraceT m) where
  getSourceResolver = lift getSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (LazyTxT QErr m) where
  getSourceResolver = lift getSourceResolver
