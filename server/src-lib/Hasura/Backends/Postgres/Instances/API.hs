{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.API where

import                          Hasura.Prelude

import                          Hasura.SQL.Backend
import                          Hasura.Server.API.Backend
import {-# SOURCE #-}           Hasura.Server.API.Metadata


instance BackendAPI ('Postgres 'Vanilla) where
  parseBackendMetadataV1 = curry $ concat <$> sequenceA
    [ sourceCommands              @('Postgres 'Vanilla)
    , tableCommands               @('Postgres 'Vanilla)
    , tablePermissionsCommands    @('Postgres 'Vanilla)
    , functionCommands            @('Postgres 'Vanilla)
    , functionPermissionsCommands @('Postgres 'Vanilla)
    , relationshipCommands        @('Postgres 'Vanilla)
    , remoteRelationshipCommands  @('Postgres 'Vanilla)
    -- postgres specific
    , sequenceA
      [ command "set_table_is_enum"    RMPgSetTableIsEnum

      , command "add_computed_field"   RMAddComputedField
      , command "drop_computed_field"  RMDropComputedField

      , command "create_event_trigger" RMPgCreateEventTrigger
      , command "delete_event_trigger" RMPgDeleteEventTrigger
      , command "redeliver_event"      RMPgRedeliverEvent
      , command "invoke_event_trigger" RMPgInvokeEventTrigger
      ]
    ]

instance BackendAPI ('Postgres 'Citus) where
  parseBackendMetadataV1 = curry $ concat <$> sequenceA
    [ sourceCommands              @('Postgres 'Citus)
    , tableCommands               @('Postgres 'Citus)
    , tablePermissionsCommands    @('Postgres 'Citus)
    , functionCommands            @('Postgres 'Citus)
    , functionPermissionsCommands @('Postgres 'Citus)
    , relationshipCommands        @('Postgres 'Citus)
    , remoteRelationshipCommands  @('Postgres 'Citus)
    ]
