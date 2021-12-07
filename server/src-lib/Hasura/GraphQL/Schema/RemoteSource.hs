module Hasura.GraphQL.Schema.RemoteSource
  ( remoteSourceField,
  )
where

import Control.Lens
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common (RelType (..), relNameToTxt)
import Hasura.RQL.Types.Relationships.FromSource
import Hasura.RQL.Types.SourceCustomization (mkCustomizedTypename)
import Hasura.SQL.AnyBackend
import Language.GraphQL.Draft.Syntax qualified as G

-- | TODO(jkachmar): Documentation.
remoteSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceFieldInfo b) ->
  m [FieldParser n (AnnotatedField b)]
remoteSourceField remoteDB = dispatchAnyBackend @BackendSchema remoteDB buildField
  where
    buildField ::
      forall src tgt.
      (BackendSchema tgt) =>
      RemoteSourceFieldInfo src tgt ->
      m [FieldParser n (AnnotatedField src)]
    buildField RemoteSourceFieldInfo {..} =
      withTypenameCustomization (mkCustomizedTypename $ Just _rsfiSourceCustomization) do
        tableInfo <- askTableInfo @tgt _rsfiSource _rsfiTable
        fieldName <- textToName $ relNameToTxt _rsfiName
        maybePerms <- tableSelectPermissions @tgt tableInfo
        case maybePerms of
          Nothing -> pure []
          Just tablePerms -> do
            parsers <- case _rsfiType of
              ObjRel -> do
                selectionSetParser <- tableSelectionSet _rsfiSource tableInfo tablePerms
                pure $
                  pure $
                    subselection_ fieldName Nothing selectionSetParser <&> \fields ->
                      IR.SourceRelationshipObject $
                        IR.AnnObjectSelectG fields _rsfiTable $
                          IR._tpFilter $
                            tablePermissionsInfo tablePerms
              ArrRel -> do
                let aggFieldName = fieldName <> $$(G.litName "_aggregate")
                selectionSetParser <- selectTable _rsfiSource tableInfo fieldName Nothing tablePerms
                aggSelectionSetParser <- selectTableAggregate _rsfiSource tableInfo aggFieldName Nothing tablePerms
                pure $
                  catMaybes
                    [ Just $ selectionSetParser <&> IR.SourceRelationshipArray,
                      aggSelectionSetParser <&> fmap IR.SourceRelationshipArrayAggregate
                    ]
            pure $
              parsers <&> fmap \select ->
                IR.AFRemote $
                  IR.RemoteRelationshipSelect (fmap (\(columnInfo, _, _) -> JoinColumn (pgiColumn columnInfo) (pgiType columnInfo)) _rsfiMapping) $
                    IR.RemoteSourceField $
                      mkAnyBackend @tgt $
                        IR.RemoteSourceSelect _rsfiSource _rsfiSourceConfig select $
                          fmap (\(_, column, columnType) -> (column, columnType)) _rsfiMapping
