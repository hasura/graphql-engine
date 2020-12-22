{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions
  , mkPermissionMetadataObject
  , mkRemoteSchemaPermissionMetadataObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.Sequence                      as Seq

import           Control.Arrow.Extended
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.Incremental                 as Inc

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.Session

buildTablePermissions
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , MonadError QErr m, ArrowWriter (Seq CollectedInfo) arr)
  => ( Inc.Dependency (TableCoreCache 'Postgres)
     , FieldInfoMap (FieldInfo 'Postgres)
     , TablePermissionInputs
     ) `arr` (RolePermInfoMap 'Postgres)
buildTablePermissions = Inc.cache proc (tableCache, tableFields, tablePermissions) -> do
  let alignedPermissions = alignPermissions tablePermissions
      table = _tpiTable tablePermissions

  (| Inc.keyed (\_ (insertPermission, selectPermission, updatePermission, deletePermission) -> do
       insert <- buildPermission -< (tableCache, table, tableFields, listToMaybe insertPermission)
       select <- buildPermission -< (tableCache, table, tableFields, listToMaybe selectPermission)
       update <- buildPermission -< (tableCache, table, tableFields, listToMaybe updatePermission)
       delete <- buildPermission -< (tableCache, table, tableFields, listToMaybe deletePermission)
       returnA -< RolePermInfo insert select update delete)
   |) alignedPermissions
  where
    mkMap :: [PermDef a] -> HashMap RoleName (PermDef a)
    mkMap = mapFromL _pdRole

    alignPermissions TablePermissionInputs{..} =
      let insertsMap = M.map (\a -> ([a], [], [], [])) (mkMap _tpiInsert)
          selectsMap = M.map (\a -> ([], [a], [], [])) (mkMap _tpiSelect)
          updatesMap = M.map (\a -> ([], [], [a], [])) (mkMap _tpiUpdate)
          deletesMap = M.map (\a -> ([], [], [], [a])) (mkMap _tpiDelete)
          unionMap   = M.unionWith (<>)
      in insertsMap `unionMap` selectsMap `unionMap` updatesMap `unionMap` deletesMap

mkPermissionMetadataObject
  :: forall a. (IsPerm a)
  => QualifiedTable -> PermDef a -> MetadataObject
mkPermissionMetadataObject table permDef =
  let permType = permAccToType (permAccessor :: PermAccessor 'Postgres (PermInfo a))
      objectId = MOTableObj table $ MTOPerm (_pdRole permDef) permType
      definition = toJSON $ WithTable table permDef
  in MetadataObject objectId definition

mkRemoteSchemaPermissionMetadataObject
  :: AddRemoteSchemaPermissions
  -> MetadataObject
mkRemoteSchemaPermissionMetadataObject (AddRemoteSchemaPermissions rsName roleName defn _) =
  let objectId = MORemoteSchemaPermissions rsName roleName
  in MetadataObject objectId $ toJSON defn

withPermission
  :: forall a b c s arr. (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr, IsPerm c)
  => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
  -> arr (a, ((QualifiedTable, PermDef c), s)) (Maybe b)
withPermission f = proc (e, ((table, permission), s)) -> do
  let metadataObject = mkPermissionMetadataObject table permission
      permType = permAccToType (permAccessor :: PermAccessor 'Postgres (PermInfo c))
      roleName = _pdRole permission
      schemaObject = SOTableObj table $
                     TOPerm roleName permType
      addPermContext err = "in permission for role " <> roleName <<> ": " <> err
  (| withRecordInconsistency (
     (| withRecordDependencies (
        (| modifyErrA (f -< (e, s))
        |) (addTableContext table . addPermContext))
     |) metadataObject schemaObject)
   |) metadataObject

buildPermission
  :: ( ArrowChoice arr, Inc.ArrowCache m arr
     , ArrowWriter (Seq CollectedInfo) arr
     , MonadError QErr m, IsPerm a
     , Inc.Cacheable a
     )
  => ( Inc.Dependency (TableCoreCache 'Postgres)
     , QualifiedTable
     , FieldInfoMap (FieldInfo 'Postgres)
     , Maybe (PermDef a)
     ) `arr` Maybe (PermInfo a)
buildPermission = Inc.cache proc (tableCache, table, tableFields, maybePermission) -> do
  (| traverseA ( \permission ->
    (| withPermission (do
         bindErrorA -< when (_pdRole permission == adminRoleName) $
           throw400 ConstraintViolation "cannot define permission for admin role"
         (info, dependencies) <- liftEitherA <<< Inc.bindDepend -< runExceptT $
           runTableCoreCacheRT (buildPermInfo table tableFields permission) (tableCache)
         tellA -< Seq.fromList dependencies
         returnA -< info)
     |) (table, permission))
   |) maybePermission
  >-> (\info -> join info >- returnA)
