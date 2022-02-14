{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Metadata () where

import Hasura.Backends.MySQL.Connection qualified as MySQL
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Backend
import Hasura.SQL.Backend

instance BackendMetadata 'MySQL where
  buildComputedFieldInfo = error "buildComputedFieldInfo: MySQL backend does not support this operation yet."
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: MySQL backend does not support this operation yet."
  resolveSourceConfig = MySQL.resolveSourceConfig
  resolveDatabaseMetadata = MySQL.resolveDatabaseMetadata
  parseBoolExpOperations = error "parseBoolExpOperations: MySQL backend does not support this operation yet."
  buildFunctionInfo = error "buildFunctionInfo: MySQL backend does not support this operation yet."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: MySQL backend does not support this operation yet."
  parseCollectableType = error "parseCollectableType: MySQL backend does not support this operation yet."
  postDropSourceHook = error "postDropSourceHook: MySQL backend does not support this operation yet."
