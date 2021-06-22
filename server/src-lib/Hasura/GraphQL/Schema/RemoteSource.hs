module Hasura.GraphQL.Schema.RemoteSource where

import           Hasura.Prelude

import qualified Hasura.RQL.IR.Select                as IR

import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Instances     ()
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.SQL.AnyBackend


remoteSourceField
  :: forall b r m n
   . MonadBuildSchema b r m n
  => AnyBackend (RemoteSourceRelationshipInfo b)
  -> m (Maybe (FieldParser n (AnnotatedField b)))
remoteSourceField remoteDB = dispatchAnyBackend @BackendSchema remoteDB buildField
  where
    buildField
      :: forall src tgt
       . BackendSchema tgt
      => RemoteSourceRelationshipInfo src tgt
      -> m (Maybe (FieldParser n (AnnotatedField src)))
    buildField (RemoteSourceRelationshipInfo {..}) = runMaybeT do
      tableInfo  <- lift $ askTableInfo @tgt _rsriSource _rsriTable
      relFieldName <- lift $ textToName $ remoteRelationshipNameToText _rsriName
      tablePerms <- MaybeT $ tableSelectPermissions @tgt tableInfo
      parser     <- lift $ case _rsriType of
        ArrayRelationship -> do
          parser <- selectTable _rsriSource tableInfo relFieldName Nothing tablePerms
          pure $ parser <&> IR.SourceRelationshipArray
        ObjectRelationship -> do
          selectionSetParser <- tableSelectionSet _rsriSource tableInfo tablePerms
          pure $ subselection_ relFieldName Nothing selectionSetParser <&> \fields ->
            IR.SourceRelationshipObject
              $ IR.AnnObjectSelectG fields _rsriTable
              $ IR._tpFilter
              $ tablePermissionsInfo tablePerms
      pure $ parser <&> \select -> IR.AFRemote
        $ IR.RemoteSelectSource
        $ mkAnyBackend @tgt
        $ IR.RemoteSourceSelect _rsriSource _rsriSourceConfig select _rsriMapping
