{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Metadata () where

import Hasura.Backends.BigQuery.DDL qualified as BigQuery
import Hasura.Backends.BigQuery.Schema.Introspection qualified as BigQuery (listAllTables)
import Hasura.Base.Error (Code (UnexpectedPayload), throw400)
import Hasura.NativeQuery.InterpolatedQuery (trimQueryEnd)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata (..))
import Hasura.NativeQuery.Validation (validateArgumentDeclaration)
import Hasura.Prelude
import Hasura.RQL.DDL.Relationship (defaultBuildArrayRelationshipInfo, defaultBuildObjectRelationshipInfo)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata.Backend
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (SCMSNotSupported))

instance BackendMetadata 'BigQuery where
  prepareCatalog _ = pure (RETDoNothing, SCMSNotSupported)
  buildComputedFieldInfo = BigQuery.buildComputedFieldInfo
  fetchAndValidateEnumValues = BigQuery.fetchAndValidateEnumValues
  resolveSourceConfig = BigQuery.resolveSourceConfig
  resolveDatabaseMetadata _ _ = BigQuery.resolveSource
  parseBoolExpOperations = BigQuery.parseBoolExpOperations
  buildArrayRelationshipInfo _ = defaultBuildArrayRelationshipInfo
  buildObjectRelationshipInfo _ = defaultBuildObjectRelationshipInfo
  buildFunctionInfo = BigQuery.buildFunctionInfo
  updateColumnInEventTrigger = BigQuery.updateColumnInEventTrigger
  parseCollectableType = BigQuery.parseCollectableType
  postDropSourceHook = BigQuery.postDropSourceHook
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    throw400 UnexpectedPayload "Computed fields are not supported in boolean expressions"
  supportsBeingRemoteRelationshipTarget _ = True
  listAllTables = BigQuery.listAllTables
  listAllTrackables _ = throw400 UnexpectedPayload "listAllTrackables not supported by BigQuery!"
  getTableInfo _ _ = throw400 UnexpectedPayload "get_table_info not yet supported in BigQuery!"
  validateNativeQuery _ _ _ _ _ nq = do
    validateArgumentDeclaration nq
    pure (trimQueryEnd (_nqmCode nq)) -- for now, all queries are valid
