{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Metadata () where

import qualified Hasura.Backends.Postgres.DDL      as PG

import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.SQL.Backend

instance BackendMetadata 'Postgres where
  buildComputedFieldInfo     = PG.buildComputedFieldInfo
  buildRemoteFieldInfo       = PG.buildRemoteFieldInfo
  fetchAndValidateEnumValues = PG.fetchAndValidateEnumValues
  resolveSourceConfig        = PG.resolveSourceConfig
  resolveDatabaseMetadata    = PG.resolveDatabaseMetadata
  createTableEventTrigger    = PG.createTableEventTrigger
  buildEventTriggerInfo      = PG.buildEventTriggerInfo
  parseBoolExpOperations     = PG.parseBoolExpOperations
  buildFunctionInfo          = PG.buildFunctionInfo
  updateColumnInEventTrigger = PG.updateColumnInEventTrigger
  parseCollectableType       = PG.parseCollectableType
  postDropSourceHook         = PG.postDropSourceHook
