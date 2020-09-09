{-# LANGUAGE Arrows #-}

module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions
  , mkPermissionMetadataObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.Sequence                      as Seq

import           Control.Arrow.Extended
import           Data.Aeson

import qualified Hasura.Incremental                 as Inc

import           Hasura.Db
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.Session
import           Hasura.SQL.Types

buildTablePermissions
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , ArrowWriter (Seq CollectedInfo) arr, MonadTx m )
  => SourceName
  -> ( Inc.Dependency TableCoreCache
     , QualifiedTable
     , FieldInfoMap FieldInfo
     , HashSet CatalogPermission
     ) `arr` RolePermInfoMap
buildTablePermissions source = Inc.cache proc (tableCache, tableName, tableFields, tablePermissions) ->
  (| Inc.keyed (\_ rolePermissions -> do
       let (insertPerms, selectPerms, updatePerms, deletePerms) =
             partitionPermissions rolePermissions

       insertPermInfo <- buildPermission -< (tableCache, source, tableName, tableFields, insertPerms)
       selectPermInfo <- buildPermission -< (tableCache, source, tableName, tableFields, selectPerms)
       updatePermInfo <- buildPermission -< (tableCache, source, tableName, tableFields, updatePerms)
       deletePermInfo <- buildPermission -< (tableCache, source, tableName, tableFields, deletePerms)

       returnA -< RolePermInfo
         { _permIns = insertPermInfo
         , _permSel = selectPermInfo
         , _permUpd = updatePermInfo
         , _permDel = deletePermInfo
         })
  |) (M.groupOn _cpRole tablePermissions)
  where
    partitionPermissions = flip foldr ([], [], [], []) $
      \perm (insertPerms, selectPerms, updatePerms, deletePerms) -> case _cpPermType perm of
        PTInsert -> (perm:insertPerms, selectPerms, updatePerms, deletePerms)
        PTSelect -> (insertPerms, perm:selectPerms, updatePerms, deletePerms)
        PTUpdate -> (insertPerms, selectPerms, perm:updatePerms, deletePerms)
        PTDelete -> (insertPerms, selectPerms, updatePerms, perm:deletePerms)

mkPermissionMetadataObject :: SourceName -> CatalogPermission -> MetadataObject
mkPermissionMetadataObject source (CatalogPermission qt rn pt pDef cmnt) =
  let objectId = MOTableObj qt $ MTOPerm rn pt
      definition = toJSON $ WithTable source qt $ PermDef rn pDef cmnt
  in MetadataObject objectId definition

withPermission
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
  -> arr (a, ((SourceName, CatalogPermission), s)) (Maybe b)
withPermission f = proc (e, ((source, permission), s)) -> do
  let CatalogPermission tableName roleName permType _ _ = permission
      metadataObject = mkPermissionMetadataObject source permission
      schemaObject = SOTableObj tableName $ TOPerm roleName permType
      addPermContext err = "in permission for role " <> roleName <<> ": " <> err
  (| withRecordInconsistency (
     (| withRecordDependencies (
        (| modifyErrA (f -< (e, s))
        |) (addTableContext tableName . addPermContext))
     |) metadataObject schemaObject)
   |) metadataObject

buildPermission
  :: ( ArrowChoice arr, Inc.ArrowCache m arr
     , ArrowWriter (Seq CollectedInfo) arr
     , MonadTx m, IsPerm a, FromJSON a
     )
  => ( Inc.Dependency TableCoreCache
     , SourceName
     , QualifiedTable
     , FieldInfoMap FieldInfo
     , [CatalogPermission]
     ) `arr` Maybe (PermInfo a)
buildPermission = Inc.cache proc (tableCache, source, tableName, tableFields, permissions) -> do
      (permissions >- noDuplicates (mkPermissionMetadataObject defaultSource))
  >-> (| traverseA (\permission@(CatalogPermission _ roleName _ pDef _) ->
         (| withPermission (do
              bindErrorA -< when (roleName == adminRoleName) $
                throw400 ConstraintViolation "cannot define permission for admin role"
              perm <- bindErrorA -< decodeValue pDef
              let permDef = PermDef roleName perm Nothing
              (info, dependencies) <- liftEitherA <<< Inc.bindDepend -< runExceptT $
                runTableCoreCacheRT (buildPermInfo tableName tableFields permDef) (source, tableCache)
              tellA -< Seq.fromList dependencies
              returnA -< info)
         |) (source, permission)) |)
  >-> (\info -> join info >- returnA)
