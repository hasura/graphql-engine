{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Metadata
--
-- Defines a 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Metadata () where

import Hasura.Backends.MSSQL.DDL qualified as MSSQL
import Hasura.Base.Error (throw500)
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend

instance BackendMetadata 'MSSQL where
  prepareCatalog = MSSQL.prepareCatalog
  buildComputedFieldInfo = MSSQL.buildComputedFieldInfo
  fetchAndValidateEnumValues = MSSQL.fetchAndValidateEnumValues
  resolveSourceConfig = MSSQL.resolveSourceConfig
  resolveDatabaseMetadata _ = MSSQL.resolveDatabaseMetadata
  parseBoolExpOperations = MSSQL.parseBoolExpOperations
  buildFunctionInfo = MSSQL.buildFunctionInfo
  updateColumnInEventTrigger = MSSQL.updateColumnInEventTrigger
  parseCollectableType = MSSQL.parseCollectableType
  postDropSourceHook = MSSQL.postDropSourceHook
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    throw500 "Computed fields are not yet defined for MSSQL backends"
