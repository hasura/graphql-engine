{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions
  , mkPermissionMetadataObject
  , mkRemoteSchemaPermissionMetadataObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as M
import qualified Data.HashMap.Strict.Extended           as M
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as Set
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Sequence                          as Seq

import           Control.Arrow.Extended
import           Data.Aeson
import           Data.Proxy
import           Data.Text.Extended

import qualified Hasura.Incremental                     as Inc
import qualified Hasura.SQL.AnyBackend                  as AB

import           Hasura.Incremental.Internal.Dependency ()
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Session


{- Note: [Inherited roles architecture for postgres read queries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Schema generation
--------------------

Schema generation for inherited roles is similar to the schema
generation of non-inherited roles. In the case of inherited roles,
we combine the `SelectPermInfo`s (see `combineSelectPermInfos`) of the
inherited role's role set and a new `SelectPermInfo` will be generated
which will be the select permission of the inherited role.

2. SQL generation
-----------------

See note [SQL generation for inherited roles]

3. Introspection
----------------

The columns accessible to an inherited role are explicitly set to
nullable irrespective of the nullability of the DB column to accomodate
cell value nullification.

-}

-- | This type is only used in the `combineSelectPermInfos` for
--   combining select permissions efficiently
data CombinedSelPermInfo (b :: BackendType)
  = CombinedSelPermInfo
  { cspiCols                 :: ![(M.HashMap (Column b) (Maybe (AnnColumnCaseBoolExpPartialSQL b)))]
  , cspiScalarComputedFields :: ![(M.HashMap ComputedFieldName (Maybe (AnnColumnCaseBoolExpPartialSQL b)))]
  , cspiFilter               :: ![(AnnBoolExpPartialSQL b)]
  , cspiLimit                :: !(Maybe Int)
  , cspiAllowAgg             :: !Bool
  , cspiRequiredHeaders      :: !(Set.HashSet Text)
  }

-- | combineSelectPermInfos combines multiple `SelPermInfo`s
--   into one `SelPermInfo`. Two `SelPermInfo` will
--   be combined in the following manner:
--
--   1. Columns - The `SelPermInfo` contains a hashset of the columns that are
--      accessible to the role. To combine two `SelPermInfo`s, every column of the
--      hashset is coupled with the boolean expression (filter) of the `SelPermInfo`
--      and a hash map of all the columns is created out of it, this hashmap is
--      generated for the `SelPermInfo`s that are going to be combined. These hashmaps
--      are then unioned and the values of these hashmaps are `OR`ed. When a column
--      is accessible to all the select permissions then the nullability of the column
--      is inferred from the DB column otherwise the column is explicitly marked as
--      nullable to accomodate cell-value nullification.
--   2. Scalar computed fields - Scalar computed fields work the same as Columns (#1)
--   3. Filter / Boolean expression - The filters are combined using a `BoolOr`
--   4. Limit - Limits are combined by taking the maximum of the two limits
--   5. Allow Aggregation - Aggregation is allowed, if any of the permissions allow it.
--   6. Request Headers - Request headers are concatenated
--
--   To maintain backwards compatibility, we handle the case of single select permission
--   differently i.e. we don't want the case statements that always evaluate to true with
--   the columns
--
combineSelectPermInfos
  :: forall b
   . (Backend b)
  => NE.NonEmpty (SelPermInfo b)
  -> SelPermInfo b
combineSelectPermInfos (headSelPermInfo NE.:| []) = headSelPermInfo
combineSelectPermInfos selPermInfos@(headSelPermInfo NE.:| restSelPermInfos) =
  let CombinedSelPermInfo {..}
        = foldr combine (modifySingleSelectPerm headSelPermInfo) restSelPermInfos
      mergeColumnsWithBoolExp xs
        | length selPermInfos == length xs = Nothing
        | otherwise                        = foldr combineCaseBoolExps Nothing xs
  in SelPermInfo (mergeColumnsWithBoolExp <$> M.unionsAll cspiCols)
                 (mergeColumnsWithBoolExp <$> M.unionsAll cspiScalarComputedFields)
                 (BoolOr cspiFilter)
                 cspiLimit
                 cspiAllowAgg
                 (toList cspiRequiredHeaders)
  where
    modifySingleSelectPerm :: SelPermInfo b -> CombinedSelPermInfo b
    modifySingleSelectPerm SelPermInfo {..} =
      let columnCaseBoolExp = fmap AnnColumnCaseBoolExpField spiFilter
          colsWithColCaseBoolExp = spiCols $> Just columnCaseBoolExp
          scalarCompFieldsWithColCaseBoolExp = spiScalarComputedFields $> Just columnCaseBoolExp
      in
        CombinedSelPermInfo [colsWithColCaseBoolExp]
                            [scalarCompFieldsWithColCaseBoolExp]
                            [spiFilter]
                            spiLimit
                            spiAllowAgg
                            (Set.fromList spiRequiredHeaders)

    combine :: SelPermInfo b -> CombinedSelPermInfo b -> CombinedSelPermInfo b
    combine (modifySingleSelectPerm -> lSelPermInfo) accSelPermInfo =
      CombinedSelPermInfo
      { cspiCols = (cspiCols lSelPermInfo) <> (cspiCols accSelPermInfo)
      , cspiScalarComputedFields =
        (cspiScalarComputedFields lSelPermInfo) <> (cspiScalarComputedFields accSelPermInfo)
      , cspiFilter = (cspiFilter lSelPermInfo) <> (cspiFilter accSelPermInfo)
      , cspiLimit =
          case (cspiLimit lSelPermInfo, cspiLimit accSelPermInfo) of
            (Nothing, Nothing) -> Nothing
            (Just l, Nothing)  -> Just l
            (Nothing, Just r)  -> Just r
            (Just l , Just r)  -> Just $ max l r
      , cspiAllowAgg = cspiAllowAgg lSelPermInfo || cspiAllowAgg accSelPermInfo
      , cspiRequiredHeaders = (cspiRequiredHeaders lSelPermInfo) <> (cspiRequiredHeaders accSelPermInfo)
      }

    combineCaseBoolExps l r =
      case (l, r) of
        (Nothing, Nothing)                     -> Nothing
        (Just caseBoolExp, Nothing)            -> Just caseBoolExp
        (Nothing, Just caseBoolExp)            -> Just caseBoolExp
        (Just caseBoolExpL, Just caseBoolExpR) -> Just $ BoolOr [caseBoolExpL, caseBoolExpR]

buildTablePermissions
  :: forall b arr m
   . ( ArrowChoice arr, Inc.ArrowDistribute arr, Inc.ArrowCache m arr
     , MonadError QErr m, ArrowWriter (Seq CollectedInfo) arr
     , HasServerConfigCtx m
     , BackendMetadata b
     , Inc.Cacheable (Proxy b)
     )
  => ( Proxy b
     , SourceName
     , Inc.Dependency (TableCoreCache b)
     , FieldInfoMap (FieldInfo b)
     , TablePermissionInputs b
     , InheritedRoles
     ) `arr` (RolePermInfoMap b)
buildTablePermissions = Inc.cache proc (proxy, source, tableCache, tableFields, tablePermissions, inheritedRoles) -> do
  let alignedPermissions = alignPermissions tablePermissions
      table = _tpiTable tablePermissions
  experimentalFeatures <- bindA -< _sccExperimentalFeatures <$> askServerConfigCtx
  nonInheritedRolePermissions <-
    (| Inc.keyed (\_ (insertPermission, selectPermission, updatePermission, deletePermission) -> do
       insert <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe insertPermission)
       select <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe selectPermission)
       update <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe updatePermission)
       delete <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe deletePermission)
       returnA -< RolePermInfo insert select update delete)
   |) alignedPermissions
  -- build permissions for inherited roles only when inherited roles is enabled
  let inheritedRolesMap =
        bool mempty (OMap.toHashMap inheritedRoles) $ EFInheritedRoles `elem` experimentalFeatures
  -- see [Inherited roles architecture for postgres read queries]
  inheritedRolePermissions <-
    (| Inc.keyed (\_ (AddInheritedRole _ roleSet) -> do
       let singleRoleSelectPerms =
             map ((_permSel =<<) . (`M.lookup` nonInheritedRolePermissions)) $
                  toList roleSet
           nonEmptySelPerms = NE.nonEmpty $ catMaybes singleRoleSelectPerms
           combinedSelPermInfo = combineSelectPermInfos <$> nonEmptySelPerms
       returnA -< RolePermInfo Nothing combinedSelPermInfo Nothing Nothing)
    |) inheritedRolesMap
  returnA -< nonInheritedRolePermissions <> inheritedRolePermissions
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
  :: forall b a. (Backend b, IsPerm b a)
  => SourceName -> TableName b -> PermDef a -> MetadataObject
mkPermissionMetadataObject source table permDef =
  let permType = permAccToType (permAccessor :: PermAccessor b (PermInfo b a))
      objectId = MOSourceObjId source
                   $ AB.mkAnyBackend
                   $ SMOTableObj @b table
                   $ MTOPerm (_pdRole permDef) permType
      definition = toJSON $ WithTable @b source table permDef
  in MetadataObject objectId definition

mkRemoteSchemaPermissionMetadataObject
  :: AddRemoteSchemaPermissions
  -> MetadataObject
mkRemoteSchemaPermissionMetadataObject (AddRemoteSchemaPermissions rsName roleName defn _) =
  let objectId = MORemoteSchemaPermissions rsName roleName
  in MetadataObject objectId $ toJSON defn

withPermission
  :: forall bknd a b c s arr. (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr, IsPerm bknd c, Backend bknd)
  => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
  -> ( a
     , ((SourceName, TableName bknd, PermDef c, Proxy bknd), s)
     ) `arr` (Maybe b)
withPermission f = proc (e, ((source, table, permission, _proxy), s)) -> do
  let metadataObject = mkPermissionMetadataObject @bknd source table permission
      permType = permAccToType (permAccessor :: PermAccessor bknd (PermInfo bknd c))
      roleName = _pdRole permission
      schemaObject = SOSourceObj source
                       $ AB.mkAnyBackend
                       $ SOITableObj @bknd table
                       $ TOPerm roleName permType
      addPermContext err = "in permission for role " <> roleName <<> ": " <> err
  (| withRecordInconsistency (
     (| withRecordDependencies (
        (| modifyErrA (f -< (e, s))
        |) (addTableContext @bknd table . addPermContext))
     |) metadataObject schemaObject)
   |) metadataObject

buildPermission
  :: forall b a arr m
   . ( ArrowChoice arr, Inc.ArrowCache m arr
     , ArrowWriter (Seq CollectedInfo) arr
     , MonadError QErr m, IsPerm b a
     , Inc.Cacheable a
     , Inc.Cacheable (Proxy b)
     , BackendMetadata b
     )
  => ( Proxy b
     , Inc.Dependency (TableCoreCache b)
     , SourceName
     , TableName b
     , FieldInfoMap (FieldInfo b)
     , Maybe (PermDef a)
     ) `arr` Maybe (PermInfo b a)
buildPermission = Inc.cache proc (proxy, tableCache, source, table, tableFields, maybePermission) -> do
  (| traverseA ( \permission ->
    (| withPermission (do
         bindErrorA -< when (_pdRole permission == adminRoleName) $
           throw400 ConstraintViolation "cannot define permission for admin role"
         (info, dependencies) <- liftEitherA <<< Inc.bindDepend -< runExceptT $
           runTableCoreCacheRT (buildPermInfo @b source table tableFields permission) (source, tableCache)
         tellA -< Seq.fromList dependencies
         returnA -< info)
     |) (source, table, permission, proxy))
   |) maybePermission
  >-> (\info -> join info >- returnA)
