{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Metadata () where

import Hasura.Backends.BigQuery.DDL qualified as BigQuery
import Hasura.Backends.BigQuery.Schema.Introspection qualified as BigQuery (listAllTables)
import Hasura.Base.Error (Code (UnexpectedPayload), throw400)
import Hasura.Prelude
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (SCMSNotSupported))

instance BackendMetadata 'BigQuery where
  prepareCatalog _ = pure (RETDoNothing, SCMSNotSupported)
  buildComputedFieldInfo = BigQuery.buildComputedFieldInfo
  fetchAndValidateEnumValues = BigQuery.fetchAndValidateEnumValues
  resolveSourceConfig = BigQuery.resolveSourceConfig
  resolveDatabaseMetadata _ _ = BigQuery.resolveSource
  parseBoolExpOperations = BigQuery.parseBoolExpOperations
  buildFunctionInfo = BigQuery.buildFunctionInfo
  updateColumnInEventTrigger = BigQuery.updateColumnInEventTrigger
  parseCollectableType = BigQuery.parseCollectableType
  postDropSourceHook = BigQuery.postDropSourceHook
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    throw400 UnexpectedPayload "Computed fields are not supported in boolean expressions"
  supportsBeingRemoteRelationshipTarget _ = True
  listAllTables = BigQuery.listAllTables
  validateNativeQuery _ _ _ _ = pure ()
