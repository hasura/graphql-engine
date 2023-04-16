module Hasura.RQL.DDL.Schema.Cache.Fields (addNonColumnFields) where

import Data.Aeson
import Data.Align (align)
import Data.HashMap.Strict.Extended qualified as M
import Data.HashSet qualified as HS
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Base.Error
import Hasura.Function.API
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

addNonColumnFields ::
  forall b m.
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    BackendMetadata b
  ) =>
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  SourceName ->
  HashMap G.Name (TableObjectType b) ->
  HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)) ->
  FieldInfoMap (ColumnInfo b) ->
  PartiallyResolvedRemoteSchemaMap ->
  DBFunctionsMetadata b ->
  NonColumnTableInputs b ->
  m (FieldInfoMap (FieldInfo b))
addNonColumnFields allSources source customObjectTypes rawTableInfos columns remoteSchemaMap pgFunctions NonColumnTableInputs {..} = do
  objectRelationshipInfos <-
    buildInfoMapPreservingMetadataM
      _rdName
      (mkRelationshipMetadataObject @b ObjRel source _nctiTable)
      (buildObjectRelationship (_tciForeignKeys <$> rawTableInfos) source _nctiTable)
      _nctiObjectRelationships

  arrayRelationshipInfos <-
    buildInfoMapPreservingMetadataM
      _rdName
      (mkRelationshipMetadataObject @b ArrRel source _nctiTable)
      (buildArrayRelationship (_tciForeignKeys <$> rawTableInfos) source _nctiTable)
      _nctiArrayRelationships

  let relationshipInfos = objectRelationshipInfos <> arrayRelationshipInfos

  computedFieldInfos <-
    buildInfoMapPreservingMetadataM
      _cfmName
      (mkComputedFieldMetadataObject source _nctiTable)
      (buildComputedField (HS.fromList $ M.keys rawTableInfos) (HS.fromList $ map ciColumn $ M.elems columns) source pgFunctions _nctiTable)
      _nctiComputedFields
  -- the fields that can be used for defining join conditions to other sources/remote schemas:
  -- 1. all columns
  -- 2. computed fields which don't expect arguments other than the table row and user session
  let lhsJoinFields =
        let columnFields = columns <&> \columnInfo -> JoinColumn (ciColumn columnInfo) (ciType columnInfo)
            computedFields = M.fromList $
              flip mapMaybe (M.toList computedFieldInfos) $
                \(cfName, (ComputedFieldInfo {..}, _)) -> do
                  scalarType <- case computedFieldReturnType @b _cfiReturnType of
                    ReturnsScalar ty -> pure ty
                    ReturnsTable {} -> Nothing
                    ReturnsOthers {} -> Nothing
                  let ComputedFieldFunction {..} = _cfiFunction
                  case toList _cffInputArgs of
                    [] ->
                      pure $
                        (fromComputedField cfName,) $
                          JoinComputedField $
                            ScalarComputedField
                              _cfiXComputedFieldInfo
                              _cfiName
                              _cffName
                              _cffComputedFieldImplicitArgs
                              scalarType
                    _ -> Nothing
         in M.union columnFields computedFields

  rawRemoteRelationshipInfos <-
    buildInfoMapPreservingMetadataM
      _rrName
      (mkRemoteRelationshipMetadataObject @b source _nctiTable)
      (buildRemoteRelationship allSources lhsJoinFields remoteSchemaMap source _nctiTable)
      _nctiRemoteRelationships

  let relationshipFields = mapKeys fromRel relationshipInfos
      computedFieldFields = mapKeys fromComputedField computedFieldInfos
      remoteRelationshipFields = mapKeys fromRemoteRelationship rawRemoteRelationshipInfos

  -- Validation phase

  -- First, check for conflicts between non-column fields, since we can raise a better error
  -- message in terms of the two metadata objects that define them.
  let relationshipAndComputedFields = align relationshipFields computedFieldFields
  step1 <- M.traverseWithKey (noFieldConflicts FIRelationship FIComputedField) relationshipAndComputedFields
  -- Second, align with remote relationship fields
  let nonColumnFields = align (catMaybes step1) remoteRelationshipFields
  step2 <- M.traverseWithKey (noFieldConflicts id FIRemoteRelationship) nonColumnFields
  -- Next, check for conflicts with custom field names. This is easiest to do before merging with
  -- the column info itself because we have access to the information separately, and custom field
  -- names are not currently stored as a separate map (but maybe should be!).
  step3 <- noCustomFieldConflicts (catMaybes step2)
  -- Finally, check for conflicts with the columns themselves.
  let allFields = align columns (catMaybes step3)
  traverse noColumnConflicts allFields
  where
    noFieldConflicts this that fieldName = \case
      This (thisField, metadata) -> pure $ Just (this thisField, metadata)
      That (thatField, metadata) -> pure $ Just (that thatField, metadata)
      These (_, thisMetadata) (_, thatMetadata) -> do
        tell $
          Seq.singleton $
            Left $
              ConflictingObjects
                ("conflicting definitions for field " <>> fieldName)
                [thisMetadata, thatMetadata]
        pure Nothing

    noCustomFieldConflicts nonColumnFields = do
      let columnsByGQLName = mapFromL ciName $ M.elems columns
      for nonColumnFields \(fieldInfo, metadata) -> withRecordInconsistencyM metadata do
        for_ (fieldInfoGraphQLNames fieldInfo) \fieldGQLName ->
          case M.lookup fieldGQLName columnsByGQLName of
            -- Only raise an error if the GQL name isn’t the same as the Postgres column name.
            -- If they are the same, `noColumnConflicts` will catch it, and it will produce a
            -- more useful error message.
            Just columnInfo
              | toTxt (ciColumn columnInfo) /= G.unName fieldGQLName ->
                  throw400 AlreadyExists $
                    "field definition conflicts with custom field name for postgres column "
                      <>> ciColumn columnInfo
            _ -> return ()
        return (fieldInfo, metadata)

    noColumnConflicts = \case
      This columnInfo -> pure $ columnInfoToFieldInfo customObjectTypes columnInfo
      That (fieldInfo, _) -> pure $ fieldInfo
      These columnInfo (_, fieldMetadata) -> do
        recordInconsistencyM Nothing fieldMetadata "field definition conflicts with postgres column"
        pure $ FIColumn columnInfo

mkRelationshipMetadataObject ::
  forall b a.
  (ToJSON a, Backend b) =>
  RelType ->
  SourceName ->
  TableName b ->
  RelDef a ->
  MetadataObject
mkRelationshipMetadataObject relType source table relDef =
  let objectId =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTORel (_rdName relDef) relType
   in MetadataObject objectId $ toJSON $ WithTable @b source table relDef

buildObjectRelationship ::
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    Backend b
  ) =>
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  SourceName ->
  TableName b ->
  ObjRelDef b ->
  m (Maybe (RelInfo b))
buildObjectRelationship fkeysMap source table relDef = do
  let buildRelInfo def = objRelP2Setup source table fkeysMap def
  buildRelationship source table buildRelInfo ObjRel relDef

buildArrayRelationship ::
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    Backend b
  ) =>
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  SourceName ->
  TableName b ->
  ArrRelDef b ->
  m (Maybe (RelInfo b))
buildArrayRelationship fkeysMap source table relDef = do
  let buildRelInfo def = arrRelP2Setup fkeysMap source table def
  buildRelationship source table buildRelInfo ArrRel relDef

buildRelationship ::
  forall m b a.
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    ToJSON a,
    Backend b
  ) =>
  SourceName ->
  TableName b ->
  (RelDef a -> Either QErr (RelInfo b, Seq SchemaDependency)) ->
  RelType ->
  RelDef a ->
  m (Maybe (RelInfo b))
buildRelationship source table buildRelInfo relType relDef = do
  let relName = _rdName relDef
      metadataObject = mkRelationshipMetadataObject @b relType source table relDef
      schemaObject =
        SOSourceObj source $
          AB.mkAnyBackend $
            SOITableObj @b table $
              TORel relName
      addRelationshipContext e = "in relationship " <> relName <<> ": " <> e
  withRecordInconsistencyM metadataObject $ do
    modifyErr (addTableContext @b table . addRelationshipContext) $ do
      (info, dependencies) <- liftEither $ buildRelInfo relDef
      recordDependenciesM metadataObject schemaObject dependencies
      return info

mkComputedFieldMetadataObject ::
  forall b.
  (Backend b) =>
  SourceName ->
  TableName b ->
  ComputedFieldMetadata b ->
  MetadataObject
mkComputedFieldMetadataObject source table ComputedFieldMetadata {..} =
  let objectId =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTOComputedField _cfmName
      definition = AddComputedField @b source table _cfmName _cfmDefinition _cfmComment
   in MetadataObject objectId (toJSON definition)

buildComputedField ::
  forall b m.
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    BackendMetadata b
  ) =>
  HashSet (TableName b) ->
  HashSet (Column b) ->
  SourceName ->
  DBFunctionsMetadata b ->
  TableName b ->
  ComputedFieldMetadata b ->
  m (Maybe (ComputedFieldInfo b))
buildComputedField trackedTableNames tableColumns source pgFunctions table cf@ComputedFieldMetadata {..} = do
  let addComputedFieldContext e = "in computed field " <> _cfmName <<> ": " <> e
      function = computedFieldFunction @b _cfmDefinition

  withRecordInconsistencyM (mkComputedFieldMetadataObject source table cf) $
    modifyErr (addTableContext @b table . addComputedFieldContext) $ do
      funcDefs <-
        onNothing
          (M.lookup function pgFunctions)
          (throw400 NotExists $ "no such function exists: " <>> function)

      rawfi <- getSingleUniqueFunctionOverload @b (computedFieldFunction @b _cfmDefinition) funcDefs
      buildComputedFieldInfo trackedTableNames table tableColumns _cfmName _cfmDefinition rawfi _cfmComment

mkRemoteRelationshipMetadataObject ::
  forall b.
  Backend b =>
  SourceName ->
  TableName b ->
  RemoteRelationship ->
  MetadataObject
mkRemoteRelationshipMetadataObject source table RemoteRelationship {..} =
  let objectId =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTORemoteRelationship _rrName
   in MetadataObject objectId $
        toJSON $
          CreateFromSourceRelationship @b source table _rrName _rrDefinition

--  | This is a "thin" wrapper around 'buildRemoteFieldInfo', which only knows
-- how to construct dependencies on the RHS of the join condition, so the
-- dependencies on the remote relationship on the LHS entity are computed here
buildRemoteRelationship ::
  forall b m.
  ( MonadWriter (Seq (Either InconsistentMetadata MetadataDependency)) m,
    BackendMetadata b
  ) =>
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  M.HashMap FieldName (DBJoinField b) ->
  PartiallyResolvedRemoteSchemaMap ->
  SourceName ->
  TableName b ->
  RemoteRelationship ->
  m (Maybe (RemoteFieldInfo (DBJoinField b)))
buildRemoteRelationship allSources allColumns remoteSchemaMap source table rr@RemoteRelationship {..} = do
  let metadataObject = mkRemoteRelationshipMetadataObject @b source table rr
      schemaObj =
        SOSourceObj source $
          AB.mkAnyBackend $
            SOITableObj @b table $
              TORemoteRel _rrName
      addRemoteRelationshipContext e = "in remote relationship " <> _rrName <<> ": " <> e
  withRecordInconsistencyM metadataObject $
    modifyErr (addTableContext @b table . addRemoteRelationshipContext) $ do
      (remoteField, rhsDependencies) <-
        buildRemoteFieldInfo (tableNameToLHSIdentifier @b table) allColumns rr allSources remoteSchemaMap
      let lhsDependencies =
            -- a direct dependency on the table on which this is defined
            SchemaDependency (SOSourceObj source $ AB.mkAnyBackend $ SOITable @b table) DRTable
              -- the relationship is also dependent on all the lhs
              -- columns that are used in the join condition
              : flip map (M.elems $ _rfiLHS remoteField) \case
                JoinColumn column _ ->
                  -- TODO: shouldn't this be DRColumn??
                  mkColDep @b DRRemoteRelationship source table column
                JoinComputedField computedFieldInfo ->
                  mkComputedFieldDep @b DRRemoteRelationship source table $ _scfName computedFieldInfo
      -- Here is the essence of the function: construct dependencies on the RHS
      -- of the join condition.
      recordDependenciesM metadataObject schemaObj (Seq.fromList lhsDependencies <> rhsDependencies)
      return remoteField
