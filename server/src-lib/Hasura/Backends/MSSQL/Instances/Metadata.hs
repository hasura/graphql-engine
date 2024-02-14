{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Metadata
--
-- Defines a 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Metadata () where

import Hasura.Backends.MSSQL.DDL qualified as MSSQL
import Hasura.Backends.MSSQL.Schema.Introspection qualified as MSSQL (listAllTables)
import Hasura.Base.Error (Code (UnexpectedPayload), throw400, throw500)
import Hasura.NativeQuery.InterpolatedQuery (trimQueryEnd)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..))
import Hasura.NativeQuery.Validation (validateArgumentDeclaration)
import Hasura.Prelude
import Hasura.RQL.DDL.Relationship (defaultBuildArrayRelationshipInfo, defaultBuildObjectRelationshipInfo)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Metadata.Backend

instance BackendMetadata 'MSSQL where
  prepareCatalog = MSSQL.prepareCatalog
  buildComputedFieldInfo = MSSQL.buildComputedFieldInfo
  fetchAndValidateEnumValues = MSSQL.fetchAndValidateEnumValues
  resolveSourceConfig = MSSQL.resolveSourceConfig
  resolveDatabaseMetadata _ _ = MSSQL.resolveDatabaseMetadata
  parseBoolExpOperations = MSSQL.parseBoolExpOperations
  buildArrayRelationshipInfo _ = defaultBuildArrayRelationshipInfo
  buildObjectRelationshipInfo _ = defaultBuildObjectRelationshipInfo
  buildFunctionInfo = MSSQL.buildFunctionInfo
  updateColumnInEventTrigger = MSSQL.updateColumnInEventTrigger
  parseCollectableType = MSSQL.parseCollectableType
  postDropSourceHook = MSSQL.postDropSourceHook
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    throw500 "Computed fields are not yet defined for MSSQL backends"
  supportsBeingRemoteRelationshipTarget _ = True
  listAllTables = MSSQL.listAllTables
  listAllTrackables _ =
    throw500 "Computed fields are not yet defined for MSSQL backends"
  getTableInfo _ _ = throw400 UnexpectedPayload "get_table_info not yet supported in MSSQL!"
  validateNativeQuery _ _ _ _ _ nq = do
    validateArgumentDeclaration nq
    pure (trimQueryEnd (_nqmCode nq)) -- for now, all queries are valid
  validateStoredProcedure _ _ _ _ = pure () -- for now, all stored procedures are valid
  getStoredProcedureGraphqlName = MSSQL.getStoredProcedureGraphqlName
