{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Source
  ( -- * Metadata
    SourceInfo (..),
    BackendSourceInfo,
    SourceCache,
    unsafeSourceConfiguration,
    unsafeSourceFunctions,
    unsafeSourceInfo,
    unsafeSourceName,
    unsafeSourceTables,
    unsafeSourceLogicalModels,
    siConfiguration,
    siNativeQueries,
    siStoredProcedures,
    siLogicalModels,
    siFunctions,
    siName,
    siSourceKind,
    siQueryTagsConfig,
    siTables,
    siCustomization,
    siDbObjectsIntrospection,

    -- * Schema cache
    DBObjectsIntrospection (..),
    ScalarMap (..),

    -- * Source resolver
    SourceResolver,
    MonadResolveSource (..),
    MaintenanceModeVersion (..),

    -- * Health check
    SourceHealthCheckInfo (..),
    BackendSourceHealthCheckInfo,
    SourceHealthCheckCache,

    -- * Source pings
    SourcePingInfo (..),
    BackendSourcePingInfo,
    SourcePingCache,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson.Extended
import Data.Environment
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Database.PG.Query qualified as PG
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Logging qualified as L
import Hasura.LogicalModel.Cache (LogicalModelCache)
import Hasura.NativeQuery.Cache (NativeQueryCache)
import Hasura.Prelude
import Hasura.QueryTags.Types
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Metadata.Common (LogicalModels)
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.StoredProcedure.Cache (StoredProcedureCache)
import Hasura.Table.Cache (DBTablesMetadata, TableCache)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Metadata (FIXME: this grouping is inaccurate)

data SourceInfo b = SourceInfo
  { _siName :: SourceName,
    _siSourceKind :: BackendSourceKind b,
    _siTables :: TableCache b,
    _siFunctions :: FunctionCache b,
    _siNativeQueries :: NativeQueryCache b,
    _siStoredProcedures :: StoredProcedureCache b,
    _siLogicalModels :: LogicalModelCache b,
    _siConfiguration :: ~(SourceConfig b),
    _siQueryTagsConfig :: Maybe QueryTagsConfig,
    _siCustomization :: ResolvedSourceCustomization,
    _siDbObjectsIntrospection :: DBObjectsIntrospection b
  }

instance
  ( Backend b,
    ToJSON (TableCache b),
    ToJSON (FunctionCache b),
    ToJSON (NativeQueryCache b),
    ToJSON (StoredProcedureCache b),
    ToJSON (QueryTagsConfig),
    ToJSON (SourceCustomization)
  ) =>
  ToJSON (SourceInfo b)
  where
  toJSON (SourceInfo {..}) =
    object
      [ "name" .= _siName,
        "tables" .= _siTables,
        "functions" .= _siFunctions,
        "native_queries" .= _siNativeQueries,
        "stored_procedures" .= _siStoredProcedures,
        "configuration" .= _siConfiguration,
        "query_tags_config" .= _siQueryTagsConfig
      ]

type BackendSourceInfo = AB.AnyBackend SourceInfo

type SourceCache = HashMap SourceName BackendSourceInfo

-- Those functions cast the content of BackendSourceInfo in order to extract
-- a backend-specific SourceInfo. Ideally, those functions should NOT be used:
-- the rest of the code should be able to deal with any source, regardless of
-- backend, through usage of the appropriate typeclasses.
-- They are thus a temporary workaround as we work on generalizing code that
-- uses the schema cache.

unsafeSourceInfo :: forall b. (HasTag b) => BackendSourceInfo -> Maybe (SourceInfo b)
unsafeSourceInfo = AB.unpackAnyBackend

unsafeSourceName :: BackendSourceInfo -> SourceName
unsafeSourceName bsi = AB.dispatchAnyBackend @Backend bsi _siName

unsafeSourceTables :: forall b. (HasTag b) => BackendSourceInfo -> Maybe (TableCache b)
unsafeSourceTables = fmap _siTables . unsafeSourceInfo @b

unsafeSourceFunctions :: forall b. (HasTag b) => BackendSourceInfo -> Maybe (FunctionCache b)
unsafeSourceFunctions = fmap _siFunctions . unsafeSourceInfo @b

unsafeSourceConfiguration :: forall b. (HasTag b) => BackendSourceInfo -> Maybe (SourceConfig b)
unsafeSourceConfiguration = fmap _siConfiguration . unsafeSourceInfo @b

unsafeSourceLogicalModels :: forall b. (HasTag b) => BackendSourceInfo -> Maybe (LogicalModelCache b)
unsafeSourceLogicalModels = fmap _siLogicalModels . unsafeSourceInfo @b

--------------------------------------------------------------------------------
-- Schema cache

-- | Contains metadata (introspection) from the database, used to build the
-- schema cache.  This type only contains results of introspecting DB objects,
-- i.e. the DB types specified by tables, functions, and scalars.  Notably, it
-- does not include the additional introspection that takes place on Postgres,
-- namely reading the contents of tables used as Enum Values -- see
-- @fetchAndValidateEnumValues@.
data DBObjectsIntrospection b = DBObjectsIntrospection
  { _rsTables :: DBTablesMetadata b,
    _rsFunctions :: DBFunctionsMetadata b,
    _rsScalars :: ScalarMap b,
    _rsLogicalModels :: LogicalModels b
  }
  deriving (Eq, Generic)

instance (Backend b) => FromJSON (DBObjectsIntrospection b) where
  parseJSON = withObject "DBObjectsIntrospection" \o -> do
    -- "tables": [["<table-1>", "<table-metadata-1>"], ["<table-2>", "<table-metadata-2>"]]
    tables <- o .: "tables"
    functions <- o .: "functions"
    scalars <- o .: "scalars"
    logicalModels <- o .:? "logical_models" .!= mempty
    pure
      DBObjectsIntrospection
        { _rsTables = HashMap.fromList tables,
          _rsFunctions = HashMap.fromList functions,
          _rsScalars = ScalarMap $ HashMap.fromList scalars,
          _rsLogicalModels = InsOrdHashMap.fromList logicalModels
        }

instance (Backend b) => ToJSON (DBObjectsIntrospection b) where
  toJSON (DBObjectsIntrospection tables functions (ScalarMap scalars) logicalModels) =
    -- "tables": [["<table-1>", "<table-metadata-1>"], ["<table-2>", "<table-metadata-2>"]]
    object
      [ "tables" .= HashMap.toList tables,
        "functions" .= HashMap.toList functions,
        "scalars" .= HashMap.toList scalars,
        "logical_models" .= InsOrdHashMap.toList logicalModels
      ]

instance (L.ToEngineLog (DBObjectsIntrospection b) L.Hasura) where
  toEngineLog _ = (L.LevelDebug, L.ELTStartup, toJSON rsLog)
    where
      rsLog =
        object
          [ "kind" .= ("resolve_source" :: Text),
            "info" .= ("Successfully resolved source" :: Text)
          ]

-- | A map from GraphQL name to equivalent scalar type for a given backend.
newtype ScalarMap b = ScalarMap (HashMap G.Name (ScalarType b))
  deriving newtype (Semigroup, Monoid)

deriving stock instance (Backend b) => Eq (ScalarMap b)

--------------------------------------------------------------------------------
-- Source resolver

-- | FIXME: this should be either in 'BackendMetadata', or into a new dedicated
-- 'BackendResolve', instead of listing backends explicitly. It could also be
-- moved to the app level.
type SourceResolver b =
  Environment -> SourceName -> SourceConnConfiguration b -> IO (Either QErr (SourceConfig b))

class (Monad m) => MonadResolveSource m where
  getPGSourceResolver :: m (SourceResolver ('Postgres 'Vanilla))
  getMSSQLSourceResolver :: m (SourceResolver 'MSSQL)

instance (MonadResolveSource m) => MonadResolveSource (ExceptT e m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (ReaderT r m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (StateT s m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (Tracing.TraceT m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadResolveSource m) => MonadResolveSource (PG.TxET QErr m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

-- FIXME: why is this here?
data MaintenanceModeVersion
  = -- | should correspond to the source catalog version from which the user
    -- is migrating from
    PreviousMMVersion
  | -- | should correspond to the latest source catalog version
    CurrentMMVersion
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Source health check

data SourceHealthCheckInfo b = SourceHealthCheckInfo
  { _shciName :: SourceName,
    _shciConnection :: SourceConnConfiguration b,
    _shciHealthCheck :: HealthCheckConfig b
  }

type BackendSourceHealthCheckInfo = AB.AnyBackend SourceHealthCheckInfo

type SourceHealthCheckCache = HashMap SourceName BackendSourceHealthCheckInfo

-------------------------------------------------------------------------------
-- Source pings

data SourcePingInfo b = SourcePingInfo
  { _spiName :: SourceName,
    _spiConnection :: SourceConnConfiguration b
  }

type BackendSourcePingInfo = AB.AnyBackend SourcePingInfo

type SourcePingCache = HashMap SourceName BackendSourcePingInfo

$(makeLenses ''SourceInfo)
