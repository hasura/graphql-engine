{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Metadata () where

import Hasura.Backends.MySQL.Connection qualified as MySQL
import Hasura.Backends.MySQL.Schema.Introspection qualified as MySQL (listAllTables)
import Hasura.Prelude
import Hasura.RQL.Types.EventTrigger (RecreateEventTriggers (RETDoNothing))
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend
import Hasura.Server.Migrate.Version (SourceCatalogMigrationState (SCMSNotSupported))

instance BackendMetadata 'MySQL where
  prepareCatalog _ = pure (RETDoNothing, SCMSNotSupported)
  buildComputedFieldInfo = error "buildComputedFieldInfo: MySQL backend does not support this operation yet."
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: MySQL backend does not support this operation yet."
  resolveSourceConfig = MySQL.resolveSourceConfig
  resolveDatabaseMetadata _ _ = MySQL.resolveDatabaseMetadata
  parseBoolExpOperations = error "parseBoolExpOperations: MySQL backend does not support this operation yet."
  buildFunctionInfo = error "buildFunctionInfo: MySQL backend does not support this operation yet."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: MySQL backend does not support this operation yet."
  parseCollectableType = error "parseCollectableType: MySQL backend does not support this operation yet."
  postDropSourceHook = MySQL.postDropSourceHook
  buildComputedFieldBooleanExp _ _ _ _ _ _ =
    error "buildComputedFieldBooleanExp: MySQL backend does not support this operation yet."
  listAllTables = MySQL.listAllTables
  supportsBeingRemoteRelationshipTarget _ = False
