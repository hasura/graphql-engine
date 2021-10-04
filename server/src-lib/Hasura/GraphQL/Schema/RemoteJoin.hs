module Hasura.GraphQL.Schema.RemoteJoin
  ( sourceToSourceField,
    schemaToSourceField,
  )
where

import Data.HashMap.Strict.Extended qualified as Map
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Common (RelName (..), RelType (..), relNameToTxt, FieldName(..))
import Hasura.RQL.Types.Relationships.FromSchema
import Hasura.RQL.Types.Relationships.FromSource
import Hasura.SQL.AnyBackend
import Language.GraphQL.Draft.Syntax qualified as G

-- | TODO(jkachmar): Documentation.
sourceToSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceFieldInfo b) ->
  m [FieldParser n (AnnotatedField b)]
sourceToSourceField remoteDB = dispatchAnyBackend @BackendSchema remoteDB buildField
  where
    buildField ::
      forall src tgt.
      BackendSchema tgt =>
      RemoteSourceFieldInfo src tgt ->
      m [FieldParser n (AnnotatedField src)]
    buildField (RemoteSourceFieldInfo {..}) = do
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
                IR.RemoteSelectSource (fmap (\(lhsColumn, _, _) -> lhsColumn) _rsfiMapping) $
                  mkAnyBackend @tgt $
                    IR.RemoteSourceSelect
                      _rsfiSource
                      _rsfiSourceConfig
                      select
                      (fmap (\(_, rhsType, rhsColumn) -> (rhsType, rhsColumn)) _rsfiMapping)

-- | TODO: document the hell out of this
schemaToSourceField ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  RelName ->
  AnyBackend ResolvedFromSchemaToSourceRelationship ->
  m [FieldParser n (IR.SchemaRelationshipSelect UnpreparedValue)]
schemaToSourceField relName relInfo = dispatchAnyBackend @BackendSchema
  relInfo
  \(sourceRel :: ResolvedFromSchemaToSourceRelationship b) -> do
    let targetTableName = _rfrtsrTable sourceRel
        targetSourceName = _rfrtsrSource sourceRel
        relationshipType = _rfrtsrRelationshipType sourceRel
    targetTableInfo <- askTableInfo targetSourceName targetTableName
    -- the creation of the parsers is identical in both functions, we might want
    -- to factor it into a common function
    tableSelectPermissions targetTableInfo >>= \case
      Nothing -> pure []
      Just targetPerms -> do
        relFieldName <- textToName $ relNameToTxt relName
        parsers <- case relationshipType of
          ObjRel -> do
            let desc = Just $ G.Description "An object relationship"
            selectionSetParser <- tableSelectionSet targetSourceName targetTableInfo targetPerms
            pure $
              pure $
                subselection_ relFieldName desc selectionSetParser <&> \fields ->
                  IR.SourceRelationshipObject $
                    IR.AnnObjectSelectG fields targetTableName $
                      IR._tpFilter $ tablePermissionsInfo targetPerms
          ArrRel -> do
            let arrayRelDesc = Just $ G.Description "An array relationship"
                aggregRelDesc = Just $ G.Description "An aggregate relationship"
                aggFieldName = relFieldName <> $$(G.litName "_aggregate")
            arrayParser <- selectTable targetSourceName targetTableInfo relFieldName arrayRelDesc targetPerms
            aggregParser <- selectTableAggregate targetSourceName targetTableInfo aggFieldName aggregRelDesc targetPerms
            pure $
              catMaybes
                [ Just $ arrayParser <&> IR.SourceRelationshipArray,
                  aggregParser <&> fmap IR.SourceRelationshipArrayAggregate
                ]
        let lhsParamOfSorts = Map.keysSet $ _rfrtsrFieldMapping sourceRel
        pure $
          parsers <&> fmap \select ->
            IR.SchemaRelationshipSource lhsParamOfSorts $
              mkAnyBackend @b
                IR.RemoteSourceSelect
                  { _rssName = targetSourceName,
                    _rssConfig = _rfrtsrSourceConfig sourceRel,
                    _rssSelection = select,
                    -- TODO: make sure the customizer is playing nice with the LHS
                    _rssJoinMapping = Map.mapKeys (FieldName . G.unName) $ _rfrtsrFieldMapping sourceRel
                  }
