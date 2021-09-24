{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Metadata where

import Hasura.Backends.BigQuery.DDL qualified as BigQuery
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend

instance BackendMetadata 'BigQuery where
  buildComputedFieldInfo = BigQuery.buildComputedFieldInfo
  fetchAndValidateEnumValues = BigQuery.fetchAndValidateEnumValues
  resolveSourceConfig = BigQuery.resolveSourceConfig
  resolveDatabaseMetadata = BigQuery.resolveSource
  parseBoolExpOperations = BigQuery.parseBoolExpOperations
  buildFunctionInfo = BigQuery.buildFunctionInfo
  updateColumnInEventTrigger = BigQuery.updateColumnInEventTrigger
  parseCollectableType = BigQuery.parseCollectableType
  postDropSourceHook = BigQuery.postDropSourceHook
