-- | BackendAPI
--
-- This module defines the 'BackendAPI' class, alongside a few helpers. Its goal is to delegate to
-- backends the responsibility of creating the parsers for the metadata API. Each backend is expected
-- to provide a list of 'CommandParser', which in turn is a simple function from command name and
-- command arguments to a corresponding parser. Command parsers can easily be created using the
-- 'commandParser' function.
--
-- Furthermore, for each set of related features, such as table tracking commands, or permission
-- commands, a helper function is provided, that allows a backend to write its instance by simply
-- listing the set of features it supports.
module Hasura.Server.API.Backend
  ( BackendAPI (..),
    commandParser,
    eventTriggerCommands,
    functionCommands,
    functionPermissionsCommands,
    relationshipCommands,
    remoteRelationshipCommands,
    sourceCommands,
    tableCommands,
    tablePermissionsCommands,
    computedFieldCommands,
  )
where

import Data.Aeson ((<?>))
import Data.Aeson.Extended (FromJSONWithContext (..))
import Data.Aeson.Types (modifyFailure)
import Data.Aeson.Types qualified as J
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.AnyBackend
import Hasura.SQL.Backend
import {-# SOURCE #-} Hasura.Server.API.Metadata

-- API class

type CommandParser b = BackendSourceKind b -> Text -> J.Value -> J.Parser (Maybe RQLMetadataV1)

class BackendAPI (b :: BackendType) where
  metadataV1CommandParsers :: [CommandParser b]

-- helpers

commandParserWithExplicitParser ::
  -- | Explicit parsing function that also takes a BackendKind
  (BackendSourceKind b -> J.Value -> J.Parser a) ->
  -- | expected command name
  Text ->
  -- | corresponding parser
  (a -> RQLMetadataV1) ->
  CommandParser b
commandParserWithExplicitParser parseJSONWithBackendKind expected constructor backendKind provided arguments =
  -- We return a Maybe parser here if the command name doesn't match, as Aeson's alternative
  -- instance backtracks: if we used 'fail', we would not be able to distinguish between "this is
  -- the correct branch, the name matches, but the argument fails to parse, we must fail" and "this
  -- is not the command we were expecting here, it is fine to continue with another".
  whenMaybe (expected == provided) $
    modifyFailure withDetails $ constructor <$> (parseJSONWithBackendKind backendKind arguments <?> J.Key "args")
  where
    withDetails internalErrorMessage =
      intercalate
        "\n"
        [ "Error when parsing command " <> T.unpack expected <> ".",
          "See our documentation at https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/index.html#metadata-apis.",
          "Internal error message: " <> internalErrorMessage
        ]

commandParser ::
  J.FromJSON a =>
  -- | expected command name
  Text ->
  -- | corresponding parser
  (a -> RQLMetadataV1) ->
  CommandParser b
commandParser =
  commandParserWithExplicitParser (const J.parseJSON) -- Ignore the backend source kind and just parse using the FromJSON instance

commandParserWithBackendKind ::
  FromJSONWithContext (BackendSourceKind b) a =>
  -- | expected command name
  Text ->
  -- | corresponding parser
  (a -> RQLMetadataV1) ->
  CommandParser b
commandParserWithBackendKind =
  commandParserWithExplicitParser parseJSONWithContext

sourceCommands,
  tableCommands,
  tablePermissionsCommands,
  functionCommands,
  functionPermissionsCommands,
  relationshipCommands,
  remoteRelationshipCommands,
  eventTriggerCommands,
  computedFieldCommands ::
    forall (b :: BackendType).
    Backend b =>
    [CommandParser b]
sourceCommands =
  [ commandParserWithBackendKind "add_source" $ RMAddSource . mkAnyBackend @b,
    commandParser "drop_source" $ RMDropSource,
    commandParser "set_table_customization" $ RMSetTableCustomization . mkAnyBackend @b
  ]
tableCommands =
  [ commandParser "track_table" $ RMTrackTable . mkAnyBackend @b,
    commandParser "untrack_table" $ RMUntrackTable . mkAnyBackend @b
  ]
tablePermissionsCommands =
  [ commandParser "create_insert_permission" $ RMCreateInsertPermission . mkAnyBackend @b,
    commandParser "create_select_permission" $ RMCreateSelectPermission . mkAnyBackend @b,
    commandParser "create_update_permission" $ RMCreateUpdatePermission . mkAnyBackend @b,
    commandParser "create_delete_permission" $ RMCreateDeletePermission . mkAnyBackend @b,
    commandParser "drop_insert_permission" $ RMDropInsertPermission . mkAnyBackend @b,
    commandParser "drop_select_permission" $ RMDropSelectPermission . mkAnyBackend @b,
    commandParser "drop_update_permission" $ RMDropUpdatePermission . mkAnyBackend @b,
    commandParser "drop_delete_permission" $ RMDropDeletePermission . mkAnyBackend @b,
    commandParser "set_permission_comment" $ RMSetPermissionComment . mkAnyBackend @b
  ]
functionCommands =
  [ commandParser "track_function" $ RMTrackFunction . mkAnyBackend @b,
    commandParser "untrack_function" $ RMUntrackFunction . mkAnyBackend @b,
    commandParser "set_function_customization" $ RMSetFunctionCustomization . mkAnyBackend @b
  ]
functionPermissionsCommands =
  [ commandParser "create_function_permission" $ RMCreateFunctionPermission . mkAnyBackend @b,
    commandParser "drop_function_permission" $ RMDropFunctionPermission . mkAnyBackend @b
  ]
relationshipCommands =
  [ commandParser "create_object_relationship" $ RMCreateObjectRelationship . mkAnyBackend @b,
    commandParser "create_array_relationship" $ RMCreateArrayRelationship . mkAnyBackend @b,
    commandParser "set_relationship_comment" $ RMSetRelationshipComment . mkAnyBackend @b,
    commandParser "rename_relationship" $ RMRenameRelationship . mkAnyBackend @b,
    commandParser "drop_relationship" $ RMDropRelationship . mkAnyBackend @b
  ]
remoteRelationshipCommands =
  [ commandParser "create_remote_relationship" $ RMCreateRemoteRelationship . mkAnyBackend @b,
    commandParser "update_remote_relationship" $ RMUpdateRemoteRelationship . mkAnyBackend @b,
    commandParser "delete_remote_relationship" $ RMDeleteRemoteRelationship . mkAnyBackend @b
  ]
eventTriggerCommands =
  [ commandParser "invoke_event_trigger" $ RMInvokeEventTrigger . mkAnyBackend @b,
    commandParser "create_event_trigger" $ RMCreateEventTrigger . mkAnyBackend @b,
    commandParser "delete_event_trigger" $ RMDeleteEventTrigger . mkAnyBackend @b,
    commandParser "redeliver_event" $ RMRedeliverEvent . mkAnyBackend @b
  ]
computedFieldCommands =
  [ commandParser "add_computed_field" $ RMAddComputedField . mkAnyBackend @b,
    commandParser "drop_computed_field" $ RMDropComputedField . mkAnyBackend @b
  ]
