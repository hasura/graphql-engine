{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Fields
  (addNonColumnFields)
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as HS
import qualified Data.Sequence                      as Seq
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Arrow.Extended
import           Control.Lens                       (_3, _4, (^.))
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.Incremental                 as Inc
import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types

addNonColumnFields
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m, BackendMetadata b)
  => ( SourceName
     , HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
     , FieldInfoMap (ColumnInfo b)
     , RemoteSchemaMap
     , DBFunctionsMetadata b
     , NonColumnTableInputs b
     ) `arr` FieldInfoMap (FieldInfo b)
addNonColumnFields = proc ( source
                          , rawTableInfo
                          , columns
                          , remoteSchemaMap
                          , pgFunctions
                          , NonColumnTableInputs{..}
                          ) -> do
  objectRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rdName . (^. _4))
         (\(s, _, t, c) -> mkRelationshipMetadataObject ObjRel (s,t,c))
         buildObjectRelationship
    -< (_tciForeignKeys <$> rawTableInfo, map (source, columns, _nctiTable,) _nctiObjectRelationships)

  arrayRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rdName . (^. _3))
         (mkRelationshipMetadataObject ArrRel)
         buildArrayRelationship
    -< (_tciForeignKeys <$> rawTableInfo, map (source, _nctiTable,) _nctiArrayRelationships)

  let relationshipInfos = objectRelationshipInfos <> arrayRelationshipInfos

  computedFieldInfos
    <- buildInfoMapPreservingMetadata
         (_cfmName . (^. _4))
         (\(s, _, t, c) -> mkComputedFieldMetadataObject (s, t, c))
         buildComputedField
    -< (HS.fromList $ M.keys rawTableInfo, map (source, pgFunctions, _nctiTable,) _nctiComputedFields)

  rawRemoteRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rrmName . (^. _3))
         mkRemoteRelationshipMetadataObject
         buildRemoteRelationship
    -< ((M.elems columns, remoteSchemaMap), map (source, _nctiTable,) _nctiRemoteRelationships)

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
    >-> (\fields -> (columns, M.catMaybes fields) >- (noCustomFieldConflicts))
    -- Finally, check for conflicts with the columns themselves.
    >-> (\fields -> align columns (M.catMaybes fields) >- returnA)
    >-> (| Inc.keyed (\_ fields -> fields >- noColumnConflicts) |)
  where
    noFieldConflicts this that = proc (fieldName, fields) -> case fields of
      This (thisField, metadata) -> returnA -< Just (this thisField, metadata)
      That (thatField, metadata) -> returnA -< Just (that thatField, metadata)
      These (_, thisMetadata) (_, thatMetadata) -> do
        tellA -< Seq.singleton $ CIInconsistency $ ConflictingObjects
          ("conflicting definitions for field " <>> fieldName)
          [thisMetadata, thatMetadata]
        returnA -< Nothing

    noCustomFieldConflicts = proc (columns, nonColumnFields) -> do
      let columnsByGQLName = mapFromL pgiName $ M.elems columns
      (| Inc.keyed (\_ (fieldInfo, metadata) ->
         (| withRecordInconsistency (do
            (| traverseA_ (\fieldGQLName -> case M.lookup fieldGQLName columnsByGQLName of
                 -- Only raise an error if the GQL name isn’t the same as the Postgres column name.
                 -- If they are the same, `noColumnConflicts` will catch it, and it will produce a
                 -- more useful error message.
                 Just columnInfo | toTxt (pgiColumn columnInfo) /= G.unName fieldGQLName ->
                   throwA -< err400 AlreadyExists
                     $ "field definition conflicts with custom field name for postgres column "
                     <>> pgiColumn columnInfo
                 _ -> returnA -< ())
             |) (fieldInfoGraphQLNames fieldInfo)
            returnA -< (fieldInfo, metadata))
         |) metadata)
       |) nonColumnFields

    noColumnConflicts = proc fields -> case fields of
      This columnInfo -> returnA -< FIColumn columnInfo
      That (fieldInfo, _) -> returnA -< fieldInfo
      These columnInfo (_, fieldMetadata) -> do
        recordInconsistency -< ((Nothing, fieldMetadata), "field definition conflicts with postgres column")
        returnA -< FIColumn columnInfo

mkRelationshipMetadataObject
  :: forall a b
   . (ToJSON a, Backend b)
  => RelType -> (SourceName, TableName b, RelDef a) -> MetadataObject
mkRelationshipMetadataObject relType (source, table, relDef) =
  let objectId = MOSourceObjId source
                   $ AB.mkAnyBackend
                   $ SMOTableObj table
                   $ MTORel (_rdName relDef) relType
  in MetadataObject objectId $ toJSON $ WithTable source table relDef

buildObjectRelationship
  :: ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     , Backend b
     )
  => ( HashMap (TableName b) (HashSet (ForeignKey b))
     , ( SourceName
       , FieldInfoMap (ColumnInfo b)
       , TableName b
       , ObjRelDef b
       )
     ) `arr` Maybe (RelInfo b)
buildObjectRelationship = proc (fkeysMap, (source, columns, table, relDef)) -> do
  let buildRelInfo def = objRelP2Setup source table fkeysMap def columns
  buildRelationship -< (source, table, buildRelInfo, ObjRel, relDef)

buildArrayRelationship
  :: ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     , Backend b
     )
  => ( HashMap (TableName b) (HashSet (ForeignKey b))
     , ( SourceName
       , TableName b
       , ArrRelDef b
       )
     ) `arr` Maybe (RelInfo b)
buildArrayRelationship = proc (fkeysMap, (source, table, relDef)) -> do
  let buildRelInfo def = arrRelP2Setup fkeysMap source table def
  buildRelationship -< (source, table, buildRelInfo, ArrRel, relDef)

buildRelationship
  :: forall b arr a
   . ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     , ToJSON a
     , Backend b
     )
  => ( SourceName
     , TableName b
     , RelDef a -> Either QErr (RelInfo b, [SchemaDependency])
     , RelType
     , RelDef a
     ) `arr` Maybe (RelInfo b)
buildRelationship = proc (source, table, buildRelInfo, relType, relDef) -> do
  let relName = _rdName relDef
      metadataObject = mkRelationshipMetadataObject relType (source, table, relDef)
      schemaObject = SOSourceObj source
                       $ AB.mkAnyBackend
                       $ SOITableObj table
                       $ TORel relName
      addRelationshipContext e = "in relationship " <> relName <<> ": " <> e
  (| withRecordInconsistency (
     (| modifyErrA (do
          (info, dependencies) <- liftEitherA -< buildRelInfo relDef
          recordDependencies -< (metadataObject, schemaObject, dependencies)
          returnA -< info)
     |) (addTableContext table . addRelationshipContext))
   |) metadataObject

mkComputedFieldMetadataObject
  :: forall b
   . (Backend b)
  => (SourceName, TableName b, ComputedFieldMetadata b)
  -> MetadataObject
mkComputedFieldMetadataObject (source, table, ComputedFieldMetadata{..}) =
  let objectId = MOSourceObjId source
                   $ AB.mkAnyBackend
                   $ SMOTableObj table
                   $ MTOComputedField _cfmName
      definition = AddComputedField source table _cfmName _cfmDefinition _cfmComment
  in MetadataObject objectId (toJSON definition)

buildComputedField
  :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m, BackendMetadata b )
  => ( HashSet (TableName b)
     , (SourceName, DBFunctionsMetadata b, TableName b, ComputedFieldMetadata b)
     ) `arr` Maybe (ComputedFieldInfo b)
buildComputedField = proc (trackedTableNames, (source, pgFunctions, table, cf@ComputedFieldMetadata{..})) -> do
  let addComputedFieldContext e = "in computed field " <> _cfmName <<> ": " <> e
      function = _cfdFunction _cfmDefinition
      funcDefs = fromMaybe [] $ M.lookup function pgFunctions
  (| withRecordInconsistency (
     (| modifyErrA (do
          rawfi <- bindErrorA -< handleMultipleFunctions (_cfdFunction _cfmDefinition) funcDefs
          bindErrorA -< buildComputedFieldInfo trackedTableNames table _cfmName _cfmDefinition rawfi _cfmComment)
     |) (addTableContext table . addComputedFieldContext))
   |) (mkComputedFieldMetadataObject (source, table, cf))

mkRemoteRelationshipMetadataObject
  :: forall b
   . Backend b
  => (SourceName, TableName b, RemoteRelationshipMetadata)
  -> MetadataObject
mkRemoteRelationshipMetadataObject (source, table, RemoteRelationshipMetadata{..}) =
  let objectId = MOSourceObjId source
                   $ AB.mkAnyBackend
                   $ SMOTableObj table
                   $ MTORemoteRelationship _rrmName
      RemoteRelationshipDef{..} = _rrmDefinition
  in MetadataObject objectId $ toJSON $
     RemoteRelationship _rrmName source table _rrdHasuraFields _rrdRemoteSchema _rrdRemoteField

buildRemoteRelationship
  :: forall b arr m
   . ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m, BackendMetadata b)
  => ( ([ColumnInfo b], RemoteSchemaMap)
     , (SourceName, TableName b, RemoteRelationshipMetadata)
     ) `arr` Maybe (RemoteFieldInfo b)
buildRemoteRelationship = proc ( (pgColumns, remoteSchemaMap)
                               , (source, table, rrm@RemoteRelationshipMetadata{..})
                               ) -> do
  let metadataObject = mkRemoteRelationshipMetadataObject (source, table, rrm)
      schemaObj = SOSourceObj source
                    $ AB.mkAnyBackend
                    $ SOITableObj table
                    $ TORemoteRel _rrmName
      addRemoteRelationshipContext e = "in remote relationship" <> _rrmName <<> ": " <> e
      RemoteRelationshipDef{..} = _rrmDefinition
      remoteRelationship = RemoteRelationship _rrmName source table _rrdHasuraFields
                           _rrdRemoteSchema _rrdRemoteField
  (| withRecordInconsistency (
       (| modifyErrA (do
          (remoteField, dependencies) <- bindErrorA -< buildRemoteFieldInfo remoteRelationship pgColumns remoteSchemaMap
          recordDependencies -< (metadataObject, schemaObj, dependencies)
          returnA -< remoteField)
        |)(addTableContext table . addRemoteRelationshipContext))
   |) metadataObject
