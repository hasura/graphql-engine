{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Fields
  ( addNonColumnFields
  , mkRelationshipMetadataObject
  , mkComputedFieldMetadataObject
  ) where
import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.Sequence                      as Seq

import           Control.Arrow.Extended
import           Data.Aeson

import qualified Hasura.Incremental                 as Inc

import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types

-- see Note [Specialization of buildRebuildableSchemaCache] in Hasura.RQL.DDL.Schema.Cache
{-# SPECIALIZE addNonColumnFields
    :: CacheBuildA
    ( HashSet ForeignKey
    , HashSet QualifiedTable
    , FieldInfoMap PGColumnInfo
    , [CatalogRelation]
    , [CatalogComputedField]
    ) (FieldInfoMap FieldInfo) #-}

addNonColumnFields
  :: ( Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => ( HashSet ForeignKey -- ^ all foreign keys
     , HashSet QualifiedTable -- ^ the names of all tracked tables
     , FieldInfoMap PGColumnInfo
     , [CatalogRelation]
     , [CatalogComputedField]
     ) `arr` FieldInfoMap FieldInfo
addNonColumnFields =
  proc (foreignKeys, trackedTableNames, columns, relationships, computedFields) -> do
    relationshipInfos <-
      (| Inc.keyed (\_ relationshipsByName -> do
           maybeRelationship <- noDuplicates mkRelationshipMetadataObject -< relationshipsByName
           (\info -> join info >- returnA) <-<
             (| traverseA (\relationship -> do
                  info <- buildRelationship -< (foreignKeys, relationship)
                  returnA -< info <&> (, mkRelationshipMetadataObject relationship))
             |) maybeRelationship)
      |) (M.groupOn _crRelName relationships)

    computedFieldInfos <-
      (| Inc.keyed (\_ computedFieldsByName -> do
           maybeComputedField <- noDuplicates mkComputedFieldMetadataObject -< computedFieldsByName
           (\info -> join info >- returnA) <-<
             (| traverseA (\computedField -> do
                  info <- buildComputedField -< (trackedTableNames, computedField)
                  returnA -< info <&> (, mkComputedFieldMetadataObject computedField))
             |) maybeComputedField)
      |) (M.groupOn (_afcName . _cccComputedField) computedFields)

    let mapKey f = M.fromList . map (first f) . M.toList
        relationshipFields = mapKey fromRel $ M.catMaybes relationshipInfos
        computedFieldFields = mapKey fromComputedField $ M.catMaybes computedFieldInfos
    nonColumnFields <-
      (| Inc.keyed (\fieldName fields -> noFieldConflicts -< (fieldName, fields))
      |) (align relationshipFields computedFieldFields)

    (| Inc.keyed (\_ fields -> noColumnConflicts -< fields)
     |) (align columns (M.catMaybes nonColumnFields))
  where
    noFieldConflicts = proc (fieldName, fields) -> case fields of
      This (relationship, metadata) -> returnA -< Just (FIRelationship relationship, metadata)
      That (computedField, metadata) -> returnA -< Just (FIComputedField computedField, metadata)
      These (_, relationshipMetadata) (_, computedFieldMetadata) -> do
        tellA -< Seq.singleton $ CIInconsistency $ ConflictingObjects
          ("conflicting definitions for field " <>> fieldName)
          [relationshipMetadata, computedFieldMetadata]
        returnA -< Nothing

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
  :: ( Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , ArrowKleisli m arr, MonadError QErr m )
  => (HashSet ForeignKey, CatalogRelation) `arr` Maybe RelInfo
buildRelationship = proc (foreignKeys, relationship) -> do
  let CatalogRelation tableName rn rt rDef _ = relationship
      metadataObject = mkRelationshipMetadataObject relationship
      schemaObject = SOTableObj tableName $ TORel rn
      addRelationshipContext e = "in relationship " <> rn <<> ": " <> e
  (| withRecordInconsistency (
     (| modifyErrA (do
          (info, dependencies) <- bindErrorA -< case rt of
            ObjRel -> do
              using <- decodeValue rDef
              objRelP2Setup tableName foreignKeys (RelDef rn using Nothing)
            ArrRel -> do
              using <- decodeValue rDef
              arrRelP2Setup tableName foreignKeys (RelDef rn using Nothing)
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
  :: ( Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
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
