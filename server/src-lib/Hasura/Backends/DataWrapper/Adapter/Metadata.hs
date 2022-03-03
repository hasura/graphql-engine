{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Metadata () where

--------------------------------------------------------------------------------
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.SQL.Backend (BackendType (DataWrapper))

--------------------------------------------------------------------------------

instance BackendMetadata 'DataWrapper where
  resolveSourceConfig = error "resolveSourceConfig: not implemented for GraphQL Data Wrappers."
  resolveDatabaseMetadata = error "resolveDatabaseMetadata: not implemented for GraphQL Data Wrappers."
  parseBoolExpOperations = error "parseBoolExpOperations: not implemented for GraphQL Data Wrappers."
  parseCollectableType = error "parseCollectableType: not implemented for GraphQL Data Wrappers."
  buildComputedFieldInfo = error "buildComputedFieldInfo: not implemented for GraphQL Data Wrappers."
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: not implemented for GraphQL Data Wrappers."
  buildFunctionInfo = error "buildFunctionInfo: not implemented for GraphQL Data Wrappers."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: not implemented for GraphQL Data Wrappers."
  postDropSourceHook = error "postDropSourceHook: not implemented for GraphQL Data Wrappers."
