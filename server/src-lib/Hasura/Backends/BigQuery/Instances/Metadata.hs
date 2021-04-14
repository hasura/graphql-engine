{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |

module Hasura.Backends.BigQuery.Instances.Metadata where

import qualified Hasura.Backends.BigQuery.DDL         as BigQuery

import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.SQL.Backend


instance BackendMetadata 'BigQuery where
  buildComputedFieldInfo     = BigQuery.buildComputedFieldInfo
  buildRemoteFieldInfo       = BigQuery.buildRemoteFieldInfo
  fetchAndValidateEnumValues = BigQuery.fetchAndValidateEnumValues
  resolveSourceConfig        = BigQuery.resolveSourceConfig
  resolveDatabaseMetadata    = BigQuery.resolveSource
  createTableEventTrigger    = BigQuery.createTableEventTrigger
  buildEventTriggerInfo      = BigQuery.buildEventTriggerInfo
  parseBoolExpOperations     = BigQuery.parseBoolExpOperations
  buildFunctionInfo          = BigQuery.buildFunctionInfo
  updateColumnInEventTrigger = BigQuery.updateColumnInEventTrigger
  parseCollectableType       = BigQuery.parseCollectableType
  postDropSourceHook         = BigQuery.postDropSourceHook
