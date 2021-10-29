module Hasura.GraphQL.Schema.RemoteSource
  ( remoteSourceField,
  )
where

import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Common (RelType (..))
import Hasura.RQL.Types.RemoteRelationship
import Hasura.RQL.Types.SourceCustomization (mkCustomizedTypename)
import Hasura.SQL.AnyBackend
import Language.GraphQL.Draft.Syntax qualified as G

-- | TODO(jkachmar): Documentation.
remoteSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceRelationshipInfo b) ->
  m [FieldParser n (AnnotatedField b)]
remoteSourceField remoteDB = dispatchAnyBackend @BackendSchema remoteDB buildField
  where
    buildField ::
      forall src tgt.
      BackendSchema tgt =>
      RemoteSourceRelationshipInfo src tgt ->
      m [FieldParser n (AnnotatedField src)]
    buildField (RemoteSourceRelationshipInfo {..}) =
      withTypenameCustomization (mkCustomizedTypename $ Just _rsriSourceCustomization) do
        tableInfo <- askTableInfo @tgt _rsriSource _rsriTable
        fieldName <- textToName $ remoteRelationshipNameToText _rsriName
        maybePerms <- tableSelectPermissions @tgt tableInfo
        case maybePerms of
          Nothing -> pure []
          Just tablePerms -> do
            parsers <- case _rsriType of
              ObjRel -> do
                selectionSetParser <- tableSelectionSet _rsriSource tableInfo tablePerms
                pure $
                  pure $
                    subselection_ fieldName Nothing selectionSetParser <&> \fields ->
                      IR.SourceRelationshipObject $
                        IR.AnnObjectSelectG fields _rsriTable $
                          IR._tpFilter $
                            tablePermissionsInfo tablePerms
              ArrRel -> do
                let aggFieldName = fieldName <> $$(G.litName "_aggregate")
                selectionSetParser <- selectTable _rsriSource tableInfo fieldName Nothing tablePerms
                aggSelectionSetParser <- selectTableAggregate _rsriSource tableInfo aggFieldName Nothing tablePerms
                pure $
                  catMaybes
                    [ Just $ selectionSetParser <&> IR.SourceRelationshipArray,
                      aggSelectionSetParser <&> fmap IR.SourceRelationshipArrayAggregate
                    ]
            pure $
              parsers <&> fmap \select ->
                IR.AFRemote $
                  IR.RemoteSelectSource $
                    mkAnyBackend @tgt $
                      IR.RemoteSourceSelect _rsriSource _rsriSourceConfig select _rsriMapping
