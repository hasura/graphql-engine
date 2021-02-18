{-# LANGUAGE AllowAmbiguousTypes #-}

module Hasura.RQL.Types.Source where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Typeable                       (cast)

import qualified Hasura.Tracing                      as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.Session

data SourceInfo b
  = SourceInfo
  { _siName          :: !SourceName
  , _siTables        :: !(TableCache b)
  , _siFunctions     :: !(FunctionCache b)
  , _siConfiguration :: !(SourceConfig b)
  } deriving (Generic)
$(makeLenses ''SourceInfo)
instance Backend b => ToJSON (SourceInfo b) where
  toJSON = genericToJSON hasuraJSON

data BackendSourceInfo =
  forall b. Backend b => BackendSourceInfo (SourceInfo b)

instance ToJSON BackendSourceInfo where
  toJSON (BackendSourceInfo si) = toJSON si

type SourceCache = HashMap SourceName BackendSourceInfo

-- Those functions cast the content of BackendSourceInfo in order to extract
-- a backend-specific SourceInfo. Ideally, those functions should NOT be used:
-- the rest of the code should be able to deal with any source, regardless of
-- backend, through usage of the appropriate typeclasses.
-- They are thus a temporary workaround as we work on generalizing code that
-- uses the schema cache.

unsafeSourceInfo :: forall b. Backend b => BackendSourceInfo -> Maybe (SourceInfo b)
unsafeSourceInfo (BackendSourceInfo si) = cast si

unsafeSourceName :: BackendSourceInfo -> SourceName
unsafeSourceName (BackendSourceInfo (SourceInfo name _ _ _)) = name

unsafeSourceTables :: forall b. Backend b => BackendSourceInfo -> Maybe (TableCache b)
unsafeSourceTables = fmap _siTables . unsafeSourceInfo

unsafeSourceFunctions :: forall b. Backend b => BackendSourceInfo -> Maybe (FunctionCache b)
unsafeSourceFunctions = fmap _siFunctions . unsafeSourceInfo

unsafeSourceConfiguration :: forall b. Backend b => BackendSourceInfo -> Maybe (SourceConfig b)
unsafeSourceConfiguration = fmap _siConfiguration . unsafeSourceInfo @b


getTableRoles :: BackendSourceInfo -> [RoleName]
getTableRoles (BackendSourceInfo si) = M.keys . _tiRolePermInfoMap =<< M.elems (_siTables si)


-- | Contains Postgres connection configuration and essential metadata from the
-- database to build schema cache for tables and function.
data ResolvedSource b
  = ResolvedSource
  { _rsConfig    :: !(SourceConfig b)
  , _rsTables    :: !(DBTablesMetadata b)
  , _rsFunctions :: !(DBFunctionsMetadata b)
  , _rsPgScalars :: !(HashSet (ScalarType b))
  } deriving (Eq)

type SourceTables b = HashMap SourceName (TableCache b)

type SourceResolver =
  SourceName -> PostgresConnConfiguration -> IO (Either QErr (SourceConfig 'Postgres))

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

-- Metadata API related types
data AddPgSource
  = AddPgSource
  { _apsName          :: !SourceName
  , _apsConfiguration :: !PostgresConnConfiguration
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''AddPgSource)

data DropPgSource
  = DropPgSource
  { _dpsName    :: !SourceName
  , _dpsCascade :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''DropPgSource)

instance FromJSON DropPgSource where
  parseJSON = withObject "Object" $ \o ->
    DropPgSource <$> o .: "name" <*> o .:? "cascade" .!= False

newtype PostgresSourceName =
  PostgresSourceName {_psnName :: SourceName}
  deriving (Show, Eq)
$(deriveJSON hasuraJSON ''PostgresSourceName)
