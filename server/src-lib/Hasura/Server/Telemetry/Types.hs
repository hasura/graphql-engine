{-# LANGUAGE TemplateHaskell #-}

-- | Telemetry types
--
-- Define anonymized metrics regarding usage of various features of Hasura.
module Hasura.Server.Telemetry.Types
  ( -- * Metrics
    RelationshipMetric (..),
    PermissionMetric (..),
    ActionMetric (..),
    NativeQueriesMetrics (..),
    StoredProceduresMetrics (..),
    LogicalModelsMetrics (..),
    Metrics (..),
    SourceMetadata (..),
    HasuraTelemetry (..),
    TelemetryPayload (..),
    Topic (..),

    -- * Counters

    -- ** Local metric recording
    RequestDimensions (..),
    RequestTimings (..),

    -- *** Dimensions
    QueryType (..),
    Locality (..),
    Transport (..),

    -- ** Metric upload
    ServiceTimingMetrics (..),
    ServiceTimingMetric (..),
    RunningTimeBucket (..),
    RequestTimingsCount (..),
  )
where

import CI qualified
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Monoid (Sum (..))
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.Server.Telemetry.Counters
import Hasura.Server.Types
import Hasura.Server.Version

data RelationshipMetric = RelationshipMetric
  { _rmManual :: Int,
    _rmAuto :: Int
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''RelationshipMetric)

data PermissionMetric = PermissionMetric
  { _pmSelect :: Int,
    _pmInsert :: Int,
    _pmUpdate :: Int,
    _pmDelete :: Int,
    _pmRoles :: Int
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''PermissionMetric)

data ActionMetric = ActionMetric
  { _amSynchronous :: Int,
    _amAsynchronous :: Int,
    _amQueryActions :: Int,
    _amTypeRelationships :: Int,
    _amCustomTypes :: Int
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''ActionMetric)

data NativeQueriesMetrics = NativeQueriesMetrics
  { _nqmWithParameters :: Int,
    _nqmWithoutParameters :: Int
  }
  deriving (Show, Eq)

instance Semigroup NativeQueriesMetrics where
  a <> b =
    NativeQueriesMetrics
      (_nqmWithParameters a + _nqmWithParameters b)
      (_nqmWithoutParameters a + _nqmWithoutParameters b)

instance Monoid NativeQueriesMetrics where
  mempty = NativeQueriesMetrics 0 0

$(J.deriveToJSON hasuraJSON ''NativeQueriesMetrics)

data StoredProceduresMetrics = StoredProceduresMetrics
  { _spmWithParameters :: Int,
    _spmWithoutParameters :: Int
  }
  deriving (Show, Eq)

instance Semigroup StoredProceduresMetrics where
  a <> b =
    StoredProceduresMetrics
      (_spmWithParameters a + _spmWithParameters b)
      (_spmWithoutParameters a + _spmWithoutParameters b)

instance Monoid StoredProceduresMetrics where
  mempty = StoredProceduresMetrics 0 0

$(J.deriveToJSON hasuraJSON ''StoredProceduresMetrics)

newtype LogicalModelsMetrics = LogicalModelsMetrics
  { _lmmCount :: Int
  }
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via Sum Int

$(J.deriveToJSON hasuraJSON ''LogicalModelsMetrics)

data Metrics = Metrics
  { _mtTables :: Int,
    _mtViews :: Int,
    _mtEnumTables :: Int,
    _mtRelationships :: RelationshipMetric,
    _mtPermissions :: PermissionMetric,
    _mtEventTriggers :: Int,
    _mtFunctions :: Int,
    _mtRemoteSchemas :: Maybe Int,
    _mtServiceTimings :: Maybe ServiceTimingMetrics,
    _mtActions :: Maybe ActionMetric,
    _mtNativeQueries :: NativeQueriesMetrics,
    _mtStoredProcedures :: StoredProceduresMetrics,
    _mtLogicalModels :: LogicalModelsMetrics
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''Metrics)

data SourceMetadata = SourceMetadata
  { _smDbUid :: Maybe DbUid,
    _smBackendType :: BackendType,
    _smDbKind :: Text,
    _smDbVersion :: Maybe DbVersion
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''SourceMetadata)

data HasuraTelemetry = HasuraTelemetry
  { _htMetadataDbUid :: MetadataDbId,
    _htInstanceUid :: InstanceId,
    _htHasuraVersion :: Version,
    _htCi :: Maybe CI.CI,
    _htSourceMetadata :: SourceMetadata,
    _htMetrics :: Metrics,
    _htExperimentalFeatures :: HashSet ExperimentalFeature
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''HasuraTelemetry)

-- | The telemetry table to which we'll add telemetry.
newtype Topic = Topic {getTopic :: Text}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

data TelemetryPayload = TelemetryPayload
  { _tpTopic :: Topic,
    _tpData :: HasuraTelemetry
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''TelemetryPayload)
