{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.Metadata () where

--------------------------------------------------------------------------------
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Backend (BackendMetadata (..))
import Hasura.SQL.Backend (BackendType (Experimental))

--------------------------------------------------------------------------------

instance BackendMetadata 'Experimental where
  resolveSourceConfig = error "resolveSourceConfig: Unimplemented for Experimental backend."
  resolveDatabaseMetadata = error "resolveDatabaseMetadata: Unimplemented for Experimental backend."
  parseBoolExpOperations = error "parseBoolExpOperations: Unimplemented for Experimental backend."
  parseCollectableType = error "parseCollectableType: Unimplemented for Experimental backend."
  buildComputedFieldInfo = error "buildComputedFieldInfo: Unimplemented for Experimental backend."
  fetchAndValidateEnumValues = error "fetchAndValidateEnumValues: Unimplemented for Experimental backend."
  buildFunctionInfo = error "buildFunctionInfo: Unimplemented for Experimental backend."
  updateColumnInEventTrigger = error "updateColumnInEventTrigger: Unimplemented for Experimental backend."
  postDropSourceHook = error "postDropSourceHook: Unimplemented for Experimental backend."
