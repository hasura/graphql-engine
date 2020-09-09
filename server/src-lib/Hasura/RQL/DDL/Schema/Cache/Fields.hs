{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Fields
  ( addNonColumnFields
  , mkRelationshipMetadataObject
  , mkComputedFieldMetadataObject
  , mkRemoteRelationshipMetadataObject
  ) where
import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as HS
import qualified Data.Sequence                      as Seq
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Arrow.Extended
import           Control.Lens                       ((^.), _3)
import           Data.Aeson

import qualified Hasura.Incremental                 as Inc

import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types
import           Hasura.SQL.Types

addNonColumnFields
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => ( SourceName
     , HashMap QualifiedTable TableRawInfo
     , FieldInfoMap PGColumnInfo
     , RemoteSchemaMap
     , NonColumnTableInputs
     ) `arr` FieldInfoMap FieldInfo
addNonColumnFields = proc ( source
                          , rawTableInfo
                          , columns
                          , remoteSchemaMap
                          , NonColumnTableInputs{..}
                          ) -> do
  objectRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rdName . (^. _3))
         (mkRelationshipMetadataObject ObjRel)
         buildObjectRelationship
    -< (_tciForeignKeys <$> rawTableInfo, map (source, _nctiTable,) _nctiObjectRelationships)

  arrayRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rdName . (^. _3))
         (mkRelationshipMetadataObject ArrRel)
         buildArrayRelationship
    -< (_tciForeignKeys <$> rawTableInfo, map (source, _nctiTable,) _nctiArrayRelationships)

  let relationshipInfos = objectRelationshipInfos <> arrayRelationshipInfos

  computedFieldInfos
    <- buildInfoMapPreservingMetadata
         (_cfmName . (^. _3))
         mkComputedFieldMetadataObject
         buildComputedField
    -< (HS.fromList $ M.keys rawTableInfo, map (source, _nctiTable,) _nctiComputedFields)

  rawRemoteRelationshipInfos
    <- buildInfoMapPreservingMetadata
         (_rrmName . (^. _3))
         mkRemoteRelationshipMetadataObject
         buildRemoteRelationship
    -< ((M.elems columns, remoteSchemaMap), map (source, _nctiTable,) _nctiRemoteRelationships)

  let mapKey f = M.fromList . map (first f) . M.toList
      relationshipFields = mapKey fromRel relationshipInfos
      computedFieldFields = mapKey fromComputedField computedFieldInfos
      remoteRelationshipFields = mapKey fromRemoteRelationship rawRemoteRelationshipInfos

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
                 Just columnInfo | getPGColTxt (pgiColumn columnInfo) /= G.unName fieldGQLName ->
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
        recordInconsistency -< (fieldMetadata, "field definition conflicts with postgres column")
        returnA -< FIColumn columnInfo

mkRelationshipMetadataObject
  :: (ToJSON a)
  => RelType -> (SourceName, QualifiedTable, RelDef a) -> MetadataObject
mkRelationshipMetadataObject relType (source, table, relDef) =
  let objectId = MOSourceObjId source $ SMOTableObj table $
                 MTORel (_rdName relDef) relType
  in MetadataObject objectId $ toJSON $ WithTable source table relDef

buildObjectRelationship
  :: ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     )
  => ( HashMap QualifiedTable (HashSet ForeignKey)
     , ( SourceName
       , QualifiedTable
       , ObjRelDef
       )
     ) `arr` Maybe RelInfo
buildObjectRelationship = proc (fkeysMap, (source, table, relDef)) -> do
  let buildRelInfo def = do
        fkeys <- getTableInfo table fkeysMap
        objRelP2Setup source table fkeys def
  buildRelationship -< (source, table, buildRelInfo, ObjRel, relDef)

buildArrayRelationship
  :: ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     )
  => ( HashMap QualifiedTable (HashSet ForeignKey)
     , ( SourceName
       , QualifiedTable
       , ArrRelDef
       )
     ) `arr` Maybe RelInfo
buildArrayRelationship = proc (fkeysMap, (source, table, relDef)) -> do
  let buildRelInfo def = do
        arrRelP2Setup source fkeysMap table def
  buildRelationship -< (source, table, buildRelInfo, ArrRel, relDef)

buildRelationship
  :: ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     , ToJSON a
     )
  => ( SourceName
     , QualifiedTable
     , (RelDef a -> Either QErr (RelInfo, [SchemaDependency]))
     , RelType
     , RelDef a
     ) `arr` Maybe RelInfo
buildRelationship = proc (source, table, buildRelInfo, relType, relDef) -> do
  let relName = _rdName relDef
      metadataObject = mkRelationshipMetadataObject relType (source, table, relDef)
      schemaObject = SOSourceObj source $ SOITableObj table $ TORel relName relType
      addRelationshipContext e = "in relationship " <> relName <<> ": " <> e
  (| withRecordInconsistency (
     (| modifyErrA (do
          (info, dependencies) <- liftEitherA -< buildRelInfo relDef
          recordDependencies -< (metadataObject, schemaObject, dependencies)
          returnA -< info)
     |) (addTableContext table . addRelationshipContext))
   |) metadataObject

mkComputedFieldMetadataObject
  :: (SourceName, QualifiedTable, ComputedFieldMetadata) -> MetadataObject
mkComputedFieldMetadataObject (source, table, ComputedFieldMetadata{..}) =
  let objectId = MOSourceObjId source $ SMOTableObj table $
                 MTOComputedField _cfmName
      definition = AddComputedField source table _cfmName _cfmDefinition _cfmComment
  in MetadataObject objectId (toJSON definition)

buildComputedField
  :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => ( HashSet QualifiedTable
     , (SourceName, QualifiedTable, ComputedFieldMetadata)
     ) `arr` Maybe ComputedFieldInfo
buildComputedField = proc (trackedTableNames, (source, table, cf@ComputedFieldMetadata{..})) -> do
  let addComputedFieldContext e = "in computed field " <> _cfmName <<> ": " <> e
      funcDefs = undefined :: [RawFunctionInfo] -- TODO: Make a postgres query after modifyErrA
  (| withRecordInconsistency (
     (| modifyErrA (do
          rawfi <- bindErrorA -< handleMultipleFunctions (_cfdFunction _cfmDefinition) funcDefs
          bindErrorA -< addComputedFieldP2Setup trackedTableNames table _cfmName _cfmDefinition rawfi _cfmComment)
     |) (addTableContext table . addComputedFieldContext))
   |) (mkComputedFieldMetadataObject (source, table, cf))

mkRemoteRelationshipMetadataObject
  :: (SourceName, QualifiedTable, RemoteRelationshipMeta) -> MetadataObject
mkRemoteRelationshipMetadataObject (source, table, RemoteRelationshipMeta{..}) =
  let objectId = MOSourceObjId source $ SMOTableObj table $
                 MTORemoteRelationship _rrmName
      RemoteRelationshipDef{..} = _rrmDefinition
  in MetadataObject objectId $ toJSON $
     RemoteRelationship _rrmName source table _rrdHasuraFields _rrdRemoteSchema _rrdRemoteField

buildRemoteRelationship
  :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => ( ([PGColumnInfo], RemoteSchemaMap)
     , (SourceName, QualifiedTable, RemoteRelationshipMeta)
     ) `arr` Maybe RemoteFieldInfo
buildRemoteRelationship = proc ( (pgColumns, remoteSchemaMap)
                               , (source, table, rrm@RemoteRelationshipMeta{..})
                               ) -> do
  let metadataObject = mkRemoteRelationshipMetadataObject (source, table, rrm)
      schemaObj = SOSourceObj source $ SOITableObj table $ TORemoteRel _rrmName
      addRemoteRelationshipContext e = "in remote relationship" <> _rrmName <<> ": " <> e
      RemoteRelationshipDef{..} = _rrmDefinition
      remoteRelationship = RemoteRelationship _rrmName source table _rrdHasuraFields
                           _rrdRemoteSchema _rrdRemoteField
  (| withRecordInconsistency (
       (| modifyErrA (do
          (remoteField, dependencies) <- bindErrorA -< resolveRemoteRelationship remoteRelationship pgColumns remoteSchemaMap
          recordDependencies -< (metadataObject, schemaObj, dependencies)
          returnA -< remoteField)
        |)(addTableContext table . addRemoteRelationshipContext))
   |) metadataObject
