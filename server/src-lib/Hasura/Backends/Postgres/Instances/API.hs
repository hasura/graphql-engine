{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances API
--
-- Defines a 'Hasura.Server.API.Backend.BackendAPI' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.API () where

import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.Server.API.Backend
import Hasura.Server.API.Metadata.Types

instance BackendAPI ('Postgres 'Vanilla) where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @('Postgres 'Vanilla),
        tableCommands @('Postgres 'Vanilla),
        tablePermissionsCommands @('Postgres 'Vanilla),
        functionCommands @('Postgres 'Vanilla),
        functionPermissionsCommands @('Postgres 'Vanilla),
        relationshipCommands @('Postgres 'Vanilla),
        remoteRelationshipCommands @('Postgres 'Vanilla),
        eventTriggerCommands @('Postgres 'Vanilla),
        computedFieldCommands @('Postgres 'Vanilla),
        nativeQueriesCommands @('Postgres 'Vanilla),
        logicalModelsCommands @('Postgres 'Vanilla),
        [ commandParser
            "set_table_is_enum"
            ( RMPgSetTableIsEnum
                . mkAnyBackend @('Postgres 'Vanilla)
            )
        ],
        connectionTemplateCommands @('Postgres 'Vanilla)
      ]

instance BackendAPI ('Postgres 'Citus) where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @('Postgres 'Citus),
        tableCommands @('Postgres 'Citus),
        tablePermissionsCommands @('Postgres 'Citus),
        functionCommands @('Postgres 'Citus),
        functionPermissionsCommands @('Postgres 'Citus),
        relationshipCommands @('Postgres 'Citus),
        remoteRelationshipCommands @('Postgres 'Citus),
        connectionTemplateCommands @('Postgres 'Citus),
        nativeQueriesCommands @('Postgres 'Citus),
        logicalModelsCommands @('Postgres 'Citus)
      ]

instance BackendAPI ('Postgres 'Cockroach) where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @('Postgres 'Cockroach),
        tableCommands @('Postgres 'Cockroach),
        tablePermissionsCommands @('Postgres 'Cockroach),
        relationshipCommands @('Postgres 'Cockroach),
        remoteRelationshipCommands @('Postgres 'Cockroach),
        [ commandParser
            "set_table_is_enum"
            ( RMPgSetTableIsEnum
                . mkAnyBackend @('Postgres 'Cockroach)
            )
        ],
        connectionTemplateCommands @('Postgres 'Cockroach),
        nativeQueriesCommands @('Postgres 'Cockroach),
        logicalModelsCommands @('Postgres 'Cockroach)
      ]
