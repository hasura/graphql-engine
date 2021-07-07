{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.API where

import                          Hasura.Prelude

import                          Hasura.SQL.Backend
import                          Hasura.Server.API.Backend
import {-# SOURCE #-}           Hasura.Server.API.Metadata


instance BackendAPI ('Postgres 'Vanilla) where
  metadataV1CommandParsers = concat
    [ sourceCommands              @('Postgres 'Vanilla)
    , tableCommands               @('Postgres 'Vanilla)
    , tablePermissionsCommands    @('Postgres 'Vanilla)
    , functionCommands            @('Postgres 'Vanilla)
    , functionPermissionsCommands @('Postgres 'Vanilla)
    , relationshipCommands        @('Postgres 'Vanilla)
    , remoteRelationshipCommands  @('Postgres 'Vanilla)
    -- postgres specific
    , [ commandParser "set_table_is_enum"    RMPgSetTableIsEnum

      , commandParser "add_computed_field"   RMAddComputedField
      , commandParser "drop_computed_field"  RMDropComputedField

      , commandParser "create_event_trigger" RMPgCreateEventTrigger
      , commandParser "delete_event_trigger" RMPgDeleteEventTrigger
      , commandParser "redeliver_event"      RMPgRedeliverEvent
      , commandParser "invoke_event_trigger" RMPgInvokeEventTrigger
      ]
    ]

instance BackendAPI ('Postgres 'Citus) where
  metadataV1CommandParsers = concat
    [ sourceCommands              @('Postgres 'Citus)
    , tableCommands               @('Postgres 'Citus)
    , tablePermissionsCommands    @('Postgres 'Citus)
    , functionCommands            @('Postgres 'Citus)
    , functionPermissionsCommands @('Postgres 'Citus)
    , relationshipCommands        @('Postgres 'Citus)
    , remoteRelationshipCommands  @('Postgres 'Citus)
    ]
