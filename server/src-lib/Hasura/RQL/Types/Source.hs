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

data ResolvedSource
  = ResolvedSource
  { _rsConfig    :: !PGSourceConfig
  , _rsTables    :: !PostgresTablesMetadata
  , _rsFunctions :: !PostgresFunctionsMetadata
  , _rsPgScalars :: !(HashSet PGScalarType)
  }

type SourceTables = HashMap SourceName TableCache

-- Metadata API related types

data SourceConnSettings
  = SourceConnSettings
  { _scsMaxConnections :: !Int
  , _scsIdleTimeout    :: !Int
  , _scsRetries        :: !Int
  } deriving (Show, Eq, Lift, Generic)
$(deriveToJSON (aesonDrop 4 snakeCase) ''SourceConnSettings)
instance FromJSON SourceConnSettings where
  parseJSON = withObject "Object" $ \o ->
    SourceConnSettings
      <$> o .:? "max_connections" .!= 50
      <*> o .:? "idle_timeout" .!= 180
      <*> o .:? "retries" .!= 1

data AddPgSource
  = AddPgSource
  { _apsName                  :: !SourceName
  , _apsUrl                   :: !UrlConf
  , _apsConnectionPoolSetting :: !SourceConnSettings
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''AddPgSource)
