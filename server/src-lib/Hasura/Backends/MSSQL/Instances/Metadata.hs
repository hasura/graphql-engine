{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Metadata () where

import qualified Hasura.Backends.MSSQL.DDL         as MSSQL

import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.SQL.Backend


instance BackendMetadata 'MSSQL where
  buildComputedFieldInfo     = MSSQL.buildComputedFieldInfo
  buildRemoteFieldInfo       = MSSQL.buildRemoteFieldInfo
  fetchAndValidateEnumValues = MSSQL.fetchAndValidateEnumValues
  resolveSourceConfig        = MSSQL.resolveSourceConfig
  resolveDatabaseMetadata    = MSSQL.resolveDatabaseMetadata
  createTableEventTrigger    = MSSQL.createTableEventTrigger
  buildEventTriggerInfo      = MSSQL.buildEventTriggerInfo
  parseBoolExpOperations     = MSSQL.parseBoolExpOperations
  buildFunctionInfo          = MSSQL.buildFunctionInfo
  updateColumnInEventTrigger = MSSQL.updateColumnInEventTrigger
  parseCollectableType       = MSSQL.parseCollectableType
  postDropSourceHook         = MSSQL.postDropSourceHook
