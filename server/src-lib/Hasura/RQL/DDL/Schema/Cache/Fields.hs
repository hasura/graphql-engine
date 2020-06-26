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
import           Data.Aeson

import qualified Hasura.Incremental                 as Inc

import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

addNonColumnFields
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => ( HashMap QualifiedTable TableRawInfo
     , FieldInfoMap PGColumnInfo
     , RemoteSchemaMap
     , [CatalogRelation]
     , [CatalogComputedField]
     , [RemoteRelationship]
     ) `arr` FieldInfoMap FieldInfo
addNonColumnFields = proc (rawTableInfo, columns,remoteSchemaMap, relationships, computedFields) -> do
  relationshipInfos
    <- buildInfoMapPreservingMetadata _crRelName mkRelationshipMetadataObject buildRelationship
    -< (_tciForeignKeys <$> rawTableInfo, relationships)
  computedFieldInfos
    <- buildInfoMapPreservingMetadata
         (_afcName . _cccComputedField)
         mkComputedFieldMetadataObject
         buildComputedField
    -< (HS.fromList $ M.keys rawTableInfo, computedFields)
  rawRemoteRelationshipInfos
    <- buildInfoMapPreservingMetadata rtrName mkRemoteRelationshipMetadataObject buildRemoteRelationship
    -< ((M.elems columns, remoteSchemaMap), remoteRelationships)

  let mapKey f = M.fromList . map (first f) . M.toList
      relationshipFields = mapKey fromRel relationshipInfos
      computedFieldFields = mapKey fromComputedField computedFieldInfos
      remoteRelationshipFields = mapKey fromRemoteRelationship $
                                 M.map (\((rf, _), mo) -> (rf, mo)) rawRemoteRelationshipInfos

  -- First, check for conflicts between non-column fields, since we can raise a better error
  -- message in terms of the two metadata objects that define them.
  (align relationshipFields computedFieldFields >- returnA)
    >-> (| Inc.keyed (\fieldName fields -> (fieldName, fields) >- noFieldConflicts FIRelationship FIComputedField) |)
    -- Second, align with remote relationship fields
    -- >-> (\fields -> align (M.catMaybes fields) remoteRelationshipFields >- returnA)
    -- >-> (| Inc.keyed (\fieldName fields -> (fieldName, fields) >- noFieldConflicts id FIRemoteRelationship) |)
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

mkRelationshipMetadataObject :: CatalogRelation -> MetadataObject
mkRelationshipMetadataObject (CatalogRelation qt rn rt rDef cmnt) =
  let objectId = MOTableObj qt $ MTORel rn rt
      definition = toJSON $ WithTable qt $ RelDef rn rDef cmnt
  in MetadataObject objectId definition

buildRelationship
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => (HashMap QualifiedTable (HashSet ForeignKey), CatalogRelation) `arr` Maybe RelInfo
buildRelationship = proc (foreignKeys, relationship) -> do
  let CatalogRelation tableName rn rt rDef _ = relationship
      metadataObject = mkRelationshipMetadataObject relationship
      schemaObject = SOTableObj tableName $ TORel rn
      addRelationshipContext e = "in relationship " <> rn <<> ": " <> e
  (| withRecordInconsistency (
     (| modifyErrA (do
          (info, dependencies) <- liftEitherA -< case rt of
            ObjRel -> do
              using <- decodeValue rDef
              tableForeignKeys <- getTableInfo tableName foreignKeys
              objRelP2Setup tableName tableForeignKeys (RelDef rn using Nothing)
            ArrRel -> do
              using <- decodeValue rDef
              arrRelP2Setup foreignKeys tableName (RelDef rn using Nothing)
          recordDependencies -< (metadataObject, schemaObject, dependencies)
          returnA -< info)
     |) (addTableContext tableName . addRelationshipContext))
   |) metadataObject

mkComputedFieldMetadataObject :: CatalogComputedField -> MetadataObject
mkComputedFieldMetadataObject (CatalogComputedField column _) =
  let AddComputedField qt name _ _ = column
      objectId = MOTableObj qt $ MTOComputedField name
  in MetadataObject objectId (toJSON column)

buildComputedField
  :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => (HashSet QualifiedTable, CatalogComputedField) `arr` Maybe ComputedFieldInfo
buildComputedField = proc (trackedTableNames, computedField) -> do
  let CatalogComputedField column funcDefs = computedField
      AddComputedField qt name def comment = column
      addComputedFieldContext e = "in computed field " <> name <<> ": " <> e
  (| withRecordInconsistency (
     (| modifyErrA (do
          rawfi <- bindErrorA -< handleMultipleFunctions (_cfdFunction def) funcDefs
          bindErrorA -< addComputedFieldP2Setup trackedTableNames qt name def rawfi comment)
     |) (addTableContext qt . addComputedFieldContext))
   |) (mkComputedFieldMetadataObject computedField)

mkRemoteRelationshipMetadataObject :: RemoteRelationship -> MetadataObject
mkRemoteRelationshipMetadataObject rr =
  let objectId = MOTableObj (rtrTable rr) $ MTORemoteRelationship $ rtrName rr
  in MetadataObject objectId $ toJSON rr

buildRemoteRelationship
  :: ( ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => (([PGColumnInfo], RemoteSchemaMap), RemoteRelationship) `arr` Maybe (RemoteFieldInfo, VT.TypeMap)
buildRemoteRelationship = proc ((pgColumns, remoteSchemaMap), remoteRelationship) -> do
  let relationshipName = rtrName remoteRelationship
      tableName = rtrTable remoteRelationship
      metadataObject = mkRemoteRelationshipMetadataObject remoteRelationship
      schemaObj = SOTableObj (rtrTable remoteRelationship) $ TORemoteRel relationshipName
      addRemoteRelationshipContext e = "in remote relationship" <> relationshipName <<> ": " <> e
  (| withRecordInconsistency (
       (| modifyErrA (do
          (remoteField, typeMap, dependencies) <- bindErrorA -< resolveRemoteRelationship remoteRelationship pgColumns remoteSchemaMap
          recordDependencies -< (metadataObject, schemaObj, dependencies)
          returnA -< (remoteField, typeMap))
        |)(addTableContext tableName . addRemoteRelationshipContext))
   |) metadataObject
