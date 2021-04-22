{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Metadata () where

import           Data.Typeable

import qualified Hasura.Backends.Postgres.DDL      as PG

import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.SQL.Backend
import           Hasura.SQL.Tag


instance
  ( HasTag    ('Postgres pgKind)
  , Typeable  ('Postgres pgKind)
  ) => BackendMetadata ('Postgres pgKind) where
  buildComputedFieldInfo     = PG.buildComputedFieldInfo
  buildRemoteFieldInfo       = PG.buildRemoteFieldInfo
  fetchAndValidateEnumValues = PG.fetchAndValidateEnumValues
  resolveSourceConfig        = PG.resolveSourceConfig
  resolveDatabaseMetadata    = PG.resolveDatabaseMetadata
  createTableEventTrigger    = PG.createTableEventTrigger
  buildEventTriggerInfo      = PG.buildEventTriggerInfo @pgKind
  parseBoolExpOperations     = PG.parseBoolExpOperations
  buildFunctionInfo          = PG.buildFunctionInfo
  updateColumnInEventTrigger = PG.updateColumnInEventTrigger
  parseCollectableType       = PG.parseCollectableType
  postDropSourceHook         = PG.postDropSourceHook
