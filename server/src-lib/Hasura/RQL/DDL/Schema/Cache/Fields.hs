{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Fields (addNonColumnFields) where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Control.Lens ((^.), _3, _4)
import Data.Aeson
import Data.Align (align)
import Data.HashMap.Strict.Extended qualified as M
import Data.HashSet qualified as HS
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Base.Error
import Hasura.Incremental qualified as Inc
import Hasura.Prelude
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Function
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

addNonColumnFields ::
  forall b arr m.
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    ArrowKleisli m arr,
    MonadError QErr m,
    BackendMetadata b
  ) =>
  ( HashMap SourceName (AB.AnyBackend PartiallyResolvedSource),
    SourceName,
    HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)),
    FieldInfoMap (ColumnInfo b),
    RemoteSchemaMap,
    DBFunctionsMetadata b,
    NonColumnTableInputs b
  )
    `arr` FieldInfoMap (FieldInfo b)
addNonColumnFields =
  proc
    ( allSources,
      source,
      rawTableInfo,
      columns,
      remoteSchemaMap,
      pgFunctions,
      NonColumnTableInputs {..}
      )
  -> do
    objectRelationshipInfos <-
      buildInfoMapPreservingMetadata
        (_rdName . (^. _3))
        (\(s, t, c) -> mkRelationshipMetadataObject @b ObjRel (s, t, c))
        buildObjectRelationship
        -<
          (_tciForeignKeys <$> rawTableInfo, map (source,_nctiTable,) _nctiObjectRelationships)

    arrayRelationshipInfos <-
      buildInfoMapPreservingMetadata
        (_rdName . (^. _3))
        (mkRelationshipMetadataObject @b ArrRel)
        buildArrayRelationship
        -<
          (_tciForeignKeys <$> rawTableInfo, map (source,_nctiTable,) _nctiArrayRelationships)

    let relationshipInfos = objectRelationshipInfos <> arrayRelationshipInfos

    computedFieldInfos <-
      buildInfoMapPreservingMetadata
        (_cfmName . (^. _4))
        (\(s, _, t, c) -> mkComputedFieldMetadataObject (s, t, c))
        ( proc (a, (b, c, d, e)) -> do
            o <- interpA @(WriterT _ Identity) -< buildComputedField a b c d e
            arrM liftEither -< o
        )
        -<
          (HS.fromList $ M.keys rawTableInfo, map (source,pgFunctions,_nctiTable,) _nctiComputedFields)

    -- the fields that can be used for defining join conditions to other sources/remote schemas:
    -- 1. all columns
    -- 2. computed fields which don't expect arguments other than the table row and user session
    let lhsJoinFields =
          let columnFields = columns <&> \columnInfo -> JoinColumn (ciColumn columnInfo) (ciType columnInfo)
              computedFields = M.fromList $
                flip mapMaybe (M.toList computedFieldInfos) $
                  \(cfName, (ComputedFieldInfo {..}, _)) -> do
                    scalarType <- case _cfiReturnType of
                      CFRScalar ty -> pure ty
                      CFRSetofTable {} -> Nothing
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
                                _cffTableArgument
                                _cffSessionArgument
                                scalarType
                      _ -> Nothing
           in M.union columnFields computedFields

    rawRemoteRelationshipInfos <-
      buildInfoMapPreservingMetadata
        (_rrName . (^. _3))
        (mkRemoteRelationshipMetadataObject @b)
        ( proc ((a, b, c), d) -> do
            o <- interpA @(WriterT _ Identity) -< buildRemoteRelationship a b c d
            arrM liftEither -< o
        )
        -<
          ((allSources, lhsJoinFields, remoteSchemaMap), map (source,_nctiTable,) _nctiRemoteRelationships)

    let relationshipFields = mapKeys fromRel relationshipInfos
        computedFieldFields = mapKeys fromComputedField computedFieldInfos
        remoteRelationshipFields = mapKeys fromRemoteRelationship rawRemoteRelationshipInfos

    -- First, check for conflicts between non-column fields, since we can raise a better error
    -- message in terms of the two metadata objects that define them.
    (align relationshipFields computedFieldFields >- returnA)
      >-> (| Inc.keyed (\fieldName fields -> (fieldName, fields) >- noFieldConflicts FIRelationship FIComputedField) |)
      -- Second, align with remote relationship fields
      >-> (\fields -> align (M.catMaybes fields) remoteRelationshipFields >- returnA)
      >-> (| Inc.keyed (\fieldName fields -> (fieldName, fields) >- noFieldConflicts id FIRemoteRelationship) |)
      -- Next, check for conflicts with custom field names. This is easiest to do before merging with
      -- the column info itself because we have access to the information separately, and custom field
      -- names are not currently stored as a separate map (but maybe should be!).
      >-> (\fields -> (columns, M.catMaybes fields) >- noCustomFieldConflicts)
      -- Finally, check for conflicts with the columns themselves.
      >-> (\fields -> align columns (M.catMaybes fields) >- returnA)
      >-> (| Inc.keyed (\_ fields -> fields >- noColumnConflicts) |)
  where
    noFieldConflicts this that = proc (fieldName, fields) -> case fields of
      This (thisField, metadata) -> returnA -< Just (this thisField, metadata)
      That (thatField, metadata) -> returnA -< Just (that thatField, metadata)
      These (_, thisMetadata) (_, thatMetadata) -> do
        tellA
          -<
            Seq.singleton $
              CIInconsistency $
                ConflictingObjects
                  ("conflicting definitions for field " <>> fieldName)
                  [thisMetadata, thatMetadata]
        returnA -< Nothing

    noCustomFieldConflicts = proc (columns, nonColumnFields) -> do
      let columnsByGQLName = mapFromL ciName $ M.elems columns
      (|
        Inc.keyed
          ( \_ (fieldInfo, metadata) ->
              (|
                withRecordInconsistency
                  ( do
                      (|
                        traverseA_
                          ( \fieldGQLName -> case M.lookup fieldGQLName columnsByGQLName of
                              -- Only raise an error if the GQL name isn’t the same as the Postgres column name.
                              -- If they are the same, `noColumnConflicts` will catch it, and it will produce a
                              -- more useful error message.
                              Just columnInfo
                                | toTxt (ciColumn columnInfo) /= G.unName fieldGQLName ->
                                  throwA
                                    -<
                                      err400 AlreadyExists $
                                        "field definition conflicts with custom field name for postgres column "
                                          <>> ciColumn columnInfo
                              _ -> returnA -< ()
                          )
                        |) (fieldInfoGraphQLNames fieldInfo)
                      returnA -< (fieldInfo, metadata)
                  )
              |) metadata
          )
        |) nonColumnFields

    noColumnConflicts = proc fields -> case fields of
      This columnInfo -> returnA -< FIColumn columnInfo
      That (fieldInfo, _) -> returnA -< fieldInfo
      These columnInfo (_, fieldMetadata) -> do
        recordInconsistency -< ((Nothing, fieldMetadata), "field definition conflicts with postgres column")
        returnA -< FIColumn columnInfo

mkRelationshipMetadataObject ::
  forall b a.
  (ToJSON a, Backend b) =>
  RelType ->
  (SourceName, TableName b, RelDef a) ->
  MetadataObject
mkRelationshipMetadataObject relType (source, table, relDef) =
  let objectId =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTORel (_rdName relDef) relType
   in MetadataObject objectId $ toJSON $ WithTable @b source table relDef

buildObjectRelationship ::
  ( ArrowChoice arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Backend b
  ) =>
  ( HashMap (TableName b) (HashSet (ForeignKey b)),
    ( SourceName,
      TableName b,
      ObjRelDef b
    )
  )
    `arr` Maybe (RelInfo b)
buildObjectRelationship = proc (fkeysMap, (source, table, relDef)) -> do
  let buildRelInfo def = objRelP2Setup source table fkeysMap def
  interpA -< buildRelationship @(WriterT _ Identity) source table buildRelInfo ObjRel relDef

buildArrayRelationship ::
  ( ArrowChoice arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Backend b
  ) =>
  ( HashMap (TableName b) (HashSet (ForeignKey b)),
    ( SourceName,
      TableName b,
      ArrRelDef b
    )
  )
    `arr` Maybe (RelInfo b)
buildArrayRelationship = proc (fkeysMap, (source, table, relDef)) -> do
  let buildRelInfo def = arrRelP2Setup fkeysMap source table def
  interpA -< buildRelationship @(WriterT _ Identity) source table buildRelInfo ArrRel relDef

buildRelationship ::
  forall m b a.
  ( MonadWriter (Seq CollectedInfo) m,
    ToJSON a,
    Backend b
  ) =>
  SourceName ->
  TableName b ->
  (RelDef a -> Either QErr (RelInfo b, [SchemaDependency])) ->
  RelType ->
  RelDef a ->
  m (Maybe (RelInfo b))
buildRelationship source table buildRelInfo relType relDef = do
  let relName = _rdName relDef
      metadataObject = mkRelationshipMetadataObject @b relType (source, table, relDef)
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
  (SourceName, TableName b, ComputedFieldMetadata b) ->
  MetadataObject
mkComputedFieldMetadataObject (source, table, ComputedFieldMetadata {..}) =
  let objectId =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTOComputedField _cfmName
      definition = AddComputedField source table _cfmName _cfmDefinition _cfmComment
   in MetadataObject objectId (toJSON definition)

buildComputedField ::
  forall b m.
  ( MonadWriter (Seq CollectedInfo) m,
    BackendMetadata b
  ) =>
  HashSet (TableName b) ->
  SourceName ->
  DBFunctionsMetadata b ->
  TableName b ->
  ComputedFieldMetadata b ->
  m (Either QErr (Maybe (ComputedFieldInfo b)))
buildComputedField trackedTableNames source pgFunctions table cf@ComputedFieldMetadata {..} = runExceptT do
  let addComputedFieldContext e = "in computed field " <> _cfmName <<> ": " <> e
      function = _cfdFunction _cfmDefinition
      funcDefs = fromMaybe [] $ M.lookup function pgFunctions
  withRecordInconsistencyM (mkComputedFieldMetadataObject (source, table, cf)) $
    modifyErr (addTableContext @b table . addComputedFieldContext) $ do
      rawfi <- handleMultipleFunctions @b (_cfdFunction _cfmDefinition) funcDefs
      buildComputedFieldInfo trackedTableNames table _cfmName _cfmDefinition rawfi _cfmComment

mkRemoteRelationshipMetadataObject ::
  forall b.
  Backend b =>
  (SourceName, TableName b, RemoteRelationship) ->
  MetadataObject
mkRemoteRelationshipMetadataObject (source, table, RemoteRelationship {..}) =
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
  ( MonadWriter (Seq CollectedInfo) m,
    BackendMetadata b
  ) =>
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  M.HashMap FieldName (DBJoinField b) ->
  RemoteSchemaMap ->
  (SourceName, TableName b, RemoteRelationship) ->
  m (Either QErr (Maybe (RemoteFieldInfo (DBJoinField b))))
buildRemoteRelationship allSources allColumns remoteSchemaMap (source, table, rr@RemoteRelationship {..}) = runExceptT $ do
  let metadataObject = mkRemoteRelationshipMetadataObject @b (source, table, rr)
      schemaObj =
        SOSourceObj source $
          AB.mkAnyBackend $
            SOITableObj @b table $
              TORemoteRel _rrName
      addRemoteRelationshipContext e = "in remote relationship" <> _rrName <<> ": " <> e
  withRecordInconsistencyM metadataObject $
    modifyErr (addTableContext @b table . addRemoteRelationshipContext) $ do
      (remoteField, rhsDependencies) <-
        buildRemoteFieldInfo (tableNameToLHSIdentifier @b table) allColumns rr allSources remoteSchemaMap
      let lhsDependencies =
            -- a direct dependency on the table on which this is defined
            SchemaDependency (SOSourceObj source $ AB.mkAnyBackend $ SOITable @b table) DRTable
            -- the relationship is also dependent on all the lhs
            -- columns that are used in the join condition
            :
            flip map (M.elems $ _rfiLHS remoteField) \case
              JoinColumn column _ ->
                -- TODO: shouldn't this be DRColumn??
                mkColDep @b DRRemoteRelationship source table column
              JoinComputedField computedFieldInfo ->
                mkComputedFieldDep @b DRRemoteRelationship source table $ _scfName computedFieldInfo
      -- Here is the essence of the function: construct dependencies on the RHS
      -- of the join condition.
      recordDependenciesM metadataObject schemaObj (lhsDependencies <> rhsDependencies)
      return remoteField
