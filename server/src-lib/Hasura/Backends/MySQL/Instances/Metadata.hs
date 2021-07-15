{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Metadata where

import qualified Hasura.Backends.MySQL.Connection  as MySQL
import           Hasura.Prelude
import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.SQL.Backend

instance BackendMetadata 'MySQL where
  buildComputedFieldInfo     = error "MySQL backend does not support this operation yet."
  fetchAndValidateEnumValues = error "MySQL backend does not support this operation yet."
  resolveSourceConfig        = MySQL.resolveSourceConfig
  resolveDatabaseMetadata    = MySQL.resolveDatabaseMetadata
  createTableEventTrigger    = error "MySQL backend does not support this operation yet."
  buildEventTriggerInfo      = error "MySQL backend does not support this operation yet."
  parseBoolExpOperations     = error "MySQL backend does not support this operation yet."
  buildFunctionInfo          = error "MySQL backend does not support this operation yet."
  updateColumnInEventTrigger = error "MySQL backend does not support this operation yet."
  parseCollectableType       = error "MySQL backend does not support this operation yet."
  postDropSourceHook         = error "MySQL backend does not support this operation yet."
