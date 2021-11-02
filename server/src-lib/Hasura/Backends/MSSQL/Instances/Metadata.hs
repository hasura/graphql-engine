{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Metadata () where

import Hasura.Backends.MSSQL.DDL qualified as MSSQL
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend

instance BackendMetadata 'MSSQL where
  buildComputedFieldInfo = MSSQL.buildComputedFieldInfo
  fetchAndValidateEnumValues = MSSQL.fetchAndValidateEnumValues
  resolveSourceConfig = MSSQL.resolveSourceConfig
  resolveDatabaseMetadata = MSSQL.resolveDatabaseMetadata
  parseBoolExpOperations = MSSQL.parseBoolExpOperations
  buildFunctionInfo = MSSQL.buildFunctionInfo
  updateColumnInEventTrigger = MSSQL.updateColumnInEventTrigger
  parseCollectableType = MSSQL.parseCollectableType
  postDropSourceHook = MSSQL.postDropSourceHook
