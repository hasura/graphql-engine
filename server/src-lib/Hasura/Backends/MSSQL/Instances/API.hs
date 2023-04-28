{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances API
--
-- Defines a 'Hasura.Server.API.Backend.BackendAPI' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.API () where

import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.Server.API.Backend

instance BackendAPI 'MSSQL where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @'MSSQL,
        tableCommands @'MSSQL,
        tablePermissionsCommands @'MSSQL,
        relationshipCommands @'MSSQL,
        remoteRelationshipCommands @'MSSQL,
        eventTriggerCommands @'MSSQL,
        nativeQueriesCommands @'MSSQL,
        storedProceduresCommands @'MSSQL,
        logicalModelsCommands @'MSSQL
      ]
