{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances API
--
-- Defines a 'Hasura.Server.API.Backend.BackendAPI' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.API () where

import Hasura.Prelude
import Hasura.SQL.Backend
import Hasura.Server.API.Backend
import {-# SOURCE #-} Hasura.Server.API.Metadata

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
        -- postgres specific
        [ commandParser "set_table_is_enum" RMPgSetTableIsEnum
        ]
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
        remoteRelationshipCommands @('Postgres 'Citus)
      ]

instance BackendAPI ('Postgres 'Cockroach) where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @('Postgres 'Cockroach),
        tableCommands @('Postgres 'Cockroach),
        tablePermissionsCommands @('Postgres 'Cockroach),
        functionCommands @('Postgres 'Cockroach),
        functionPermissionsCommands @('Postgres 'Cockroach),
        relationshipCommands @('Postgres 'Cockroach),
        remoteRelationshipCommands @('Postgres 'Cockroach)
      ]
