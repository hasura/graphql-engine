{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.API () where

--------------------------------------------------------------------------------

import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (DataConnector))
import Hasura.Server.API.Backend (BackendAPI (..), functionCommands, functionPermissionsCommands, logicalModelsCommands, nativeQueriesCommands, relationshipCommands, remoteRelationshipCommands, sourceCommands, tableCommands, tablePermissionsCommands, trackableCommands)

--------------------------------------------------------------------------------

instance BackendAPI 'DataConnector where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @'DataConnector,
        tableCommands @'DataConnector,
        tablePermissionsCommands @'DataConnector,
        trackableCommands @'DataConnector,
        functionCommands @'DataConnector,
        functionPermissionsCommands @'DataConnector,
        relationshipCommands @'DataConnector,
        remoteRelationshipCommands @'DataConnector,
        logicalModelsCommands @'DataConnector,
        nativeQueriesCommands @'DataConnector
      ]
