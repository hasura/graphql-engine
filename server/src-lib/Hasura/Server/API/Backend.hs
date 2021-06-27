module Hasura.Server.API.Backend where

import                          Hasura.Prelude

import                qualified Data.Aeson.Types           as J

import                          Hasura.RQL.Types.Backend
import                          Hasura.SQL.AnyBackend
import                          Hasura.SQL.Backend
import {-# SOURCE #-}           Hasura.Server.API.Metadata


class BackendAPI (b :: BackendType) where
  parseBackendMetadataV1
    :: Text     -- ^ command name
    -> J.Value  -- ^ arguments
    -> [J.Parser RQLMetadataV1]


-- helpers

command
  :: forall a. J.FromJSON a
  => Text                  -- ^ expected command name
  -> (a -> RQLMetadataV1)  -- ^ corresponding parser
  -> ( Text                -- ^ provided command name
     , J.Value             -- ^ provided arguments
     )
  -> J.Parser RQLMetadataV1
command expected constructor (provided, arguments) = do
  guard (expected == provided)
  constructor <$> J.parseJSON arguments

sourceCommands, tableCommands, tablePermissionsCommands, functionCommands, functionPermissionsCommands, relationshipCommands, remoteRelationshipCommands
  :: forall (b :: BackendType). Backend b
  => (Text, J.Value)
  -> [J.Parser RQLMetadataV1]
sourceCommands = sequenceA
  [ command "add_source"              $ RMAddSource             . mkAnyBackend @b
  , command "drop_source"             $ RMDropSource
  , command "set_table_customization" $ RMSetTableCustomization . mkAnyBackend @b
  ]
tableCommands = sequenceA
  [ command "track_table"   $ RMTrackTable   . mkAnyBackend @b
  , command "untrack_table" $ RMUntrackTable . mkAnyBackend @b
  ]
tablePermissionsCommands = sequenceA
  [ command "create_insert_permission" $ RMCreateInsertPermission . mkAnyBackend @b
  , command "create_select_permission" $ RMCreateSelectPermission . mkAnyBackend @b
  , command "create_update_permission" $ RMCreateUpdatePermission . mkAnyBackend @b
  , command "create_delete_permission" $ RMCreateDeletePermission . mkAnyBackend @b
  , command "drop_insert_permission"   $ RMDropInsertPermission   . mkAnyBackend @b
  , command "drop_select_permission"   $ RMDropSelectPermission   . mkAnyBackend @b
  , command "drop_update_permission"   $ RMDropUpdatePermission   . mkAnyBackend @b
  , command "drop_delete_permission"   $ RMDropDeletePermission   . mkAnyBackend @b
  , command "set_permission_comment"   $ RMSetPermissionComment   . mkAnyBackend @b
  ]
functionCommands = sequenceA
  [ command "track_function"   $ RMTrackFunction   . mkAnyBackend @b
  , command "untrack_function" $ RMUntrackFunction . mkAnyBackend @b
  ]
functionPermissionsCommands = sequenceA
  [ command "create_function_permission" $ RMCreateFunctionPermission . mkAnyBackend @b
  , command "drop_function_permission"   $ RMDropFunctionPermission   . mkAnyBackend @b
  ]
relationshipCommands = sequenceA
  [ command "create_object_relationship" $ RMCreateObjectRelationship . mkAnyBackend @b
  , command "create_array_relationship"  $ RMCreateArrayRelationship  . mkAnyBackend @b
  , command "set_relationship_comment"   $ RMSetRelationshipComment   . mkAnyBackend @b
  , command "rename_relationship"        $ RMRenameRelationship       . mkAnyBackend @b
  , command "drop_relationship"          $ RMDropRelationship         . mkAnyBackend @b
  ]
remoteRelationshipCommands = sequenceA
  [ command "create_remote_relationship" $ RMCreateRemoteRelationship . mkAnyBackend @b
  , command "update_remote_relationship" $ RMUpdateRemoteRelationship . mkAnyBackend @b
  , command "delete_remote_relationship" $ RMDeleteRemoteRelationship
  ]
