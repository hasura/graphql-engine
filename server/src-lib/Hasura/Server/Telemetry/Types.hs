{-# LANGUAGE TemplateHaskell #-}

-- | Telemetry types
--
-- Define anonymized metrics regarding usage of various features of Hasura.
module Hasura.Server.Telemetry.Types
  ( -- * Metrics
    RelationshipMetric (..),
    PermissionMetric (..),
    ActionMetric (..),
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
import Data.Aeson qualified as A
import Data.Aeson.TH qualified as A
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.SQL.Backend (BackendType)
import Hasura.Server.Telemetry.Counters
import Hasura.Server.Types
import Hasura.Server.Version

data RelationshipMetric = RelationshipMetric
  { _rmManual :: Int,
    _rmAuto :: Int
  }
  deriving (Show, Eq)

$(A.deriveToJSON hasuraJSON ''RelationshipMetric)

data PermissionMetric = PermissionMetric
  { _pmSelect :: Int,
    _pmInsert :: Int,
    _pmUpdate :: Int,
    _pmDelete :: Int,
    _pmRoles :: Int
  }
  deriving (Show, Eq)

$(A.deriveToJSON hasuraJSON ''PermissionMetric)

data ActionMetric = ActionMetric
  { _amSynchronous :: Int,
    _amAsynchronous :: Int,
    _amQueryActions :: Int,
    _amTypeRelationships :: Int,
    _amCustomTypes :: Int
  }
  deriving (Show, Eq)

$(A.deriveToJSON hasuraJSON ''ActionMetric)

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
    _mtActions :: Maybe ActionMetric
  }
  deriving (Show, Eq)

$(A.deriveToJSON hasuraJSON ''Metrics)

data SourceMetadata = SourceMetadata
  { _smDbUid :: Maybe DbUid,
    _smDbKind :: BackendType,
    _smDbVersion :: Maybe DbVersion
  }
  deriving (Show, Eq)

$(A.deriveToJSON hasuraJSON ''SourceMetadata)

data HasuraTelemetry = HasuraTelemetry
  { _htMetadataDbUid :: MetadataDbId,
    _htInstanceUid :: InstanceId,
    _htHasuraVersion :: Version,
    _htCi :: Maybe CI.CI,
    _htSourceMetadata :: SourceMetadata,
    _htMetrics :: Metrics
  }
  deriving (Show)

$(A.deriveToJSON hasuraJSON ''HasuraTelemetry)

-- | The telemetry table to which we'll add telemetry.
newtype Topic = Topic {getTopic :: Text}
  deriving (Show, Eq, A.ToJSON, A.FromJSON)

data TelemetryPayload = TelemetryPayload
  { _tpTopic :: Topic,
    _tpData :: HasuraTelemetry
  }
  deriving (Show)

$(A.deriveToJSON hasuraJSON ''TelemetryPayload)
