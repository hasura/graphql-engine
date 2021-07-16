{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions
  , mkPermissionMetadataObject
  , mkRemoteSchemaPermissionMetadataObject
  , orderRoles
  ) where

import           Hasura.Prelude

import qualified Data.Graph                         as G
import qualified Data.HashMap.Strict                as M
import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as Set
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Sequence                      as Seq

import           Control.Arrow.Extended
import           Data.Aeson
import           Data.Proxy
import           Data.Text.Extended

import qualified Hasura.Incremental                 as Inc
import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.Base.Error
import           Hasura.RQL.DDL.Permission
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

-- | 'orderRoles' is used to order the roles, in such a way that given
--   a role R with n parent roles - PR1, PR2 .. PRn, then the 'orderRoles'
--   function will order the roles in such a way that all the parent roles
--   precede the role R. Note that the order of the parent roles itself doesn't
--   matter as long as they precede the roles on which they are dependent on.
--
--   For example, the orderRoles may return `[PR1, PR3, PR2, ... PRn, R]`
--   or `[PR5, PR3, PR1 ... R]`, both of them are correct because all
--   the parent roles precede the inherited role R, assuming the parent roles
--   themselves don't have any parents for the sake of this example.
orderRoles
  :: MonadError QErr m
  => [Role]
  -> m [Role]
orderRoles allRoles = do
  -- inherited roles can be created from other inherited and non-inherited roles
  -- So, roles can be thought of as a graph where non-inherited roles don't have
  -- any outgoing edges and inherited roles as nodes with edges to its parent roles
  -- However, we can't allow cyclic roles since permissions built by a role is used
  -- by the dependent roles to build their permissions and if cyclic roles were to be
  -- allowed, the permissions building will be stuck in an infinite loop
  let graphNodesList = [(role, _rRoleName role, toList (_unParentRoles .  _rParentRoles $ role)) | role <- allRoles]
  let orderedGraphNodes = G.stronglyConnComp graphNodesList -- topologically sort the nodes of the graph
      cyclicRoles = filter checkCycle orderedGraphNodes
  unless (null cyclicRoles) $ do
    -- we're appending the first element of the list at the end, so that the error message will
    -- contain the complete cycle of the roles
    let roleCycles = map (tshow . map (roleNameToTxt . _rRoleName) . appendFirstElementAtEnd .  G.flattenSCC) cyclicRoles
    throw400 CyclicDependency $ "found cycle(s) in roles: " <> commaSeparated roleCycles
  let allOrderedRoles = G.flattenSCCs orderedGraphNodes
  pure allOrderedRoles
  where
    checkCycle = \case
      G.AcyclicSCC _ -> False
      G.CyclicSCC  _ -> True

    appendFirstElementAtEnd []     = []
    appendFirstElementAtEnd (x:xs) = (x:xs) ++ [x]

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
     , [Role]
     ) `arr` (RolePermInfoMap b)
buildTablePermissions = Inc.cache proc (proxy, source, tableCache, tableFields, tablePermissions, orderedRoles) -> do
  let alignedPermissions = alignPermissions tablePermissions
      table = _tpiTable tablePermissions
  experimentalFeatures <- bindA -< _sccExperimentalFeatures <$> askServerConfigCtx
  metadataRolePermissions <-
    (| Inc.keyed (\_ (insertPermission, selectPermission, updatePermission, deletePermission) -> do
       insert <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe insertPermission)
       select <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe selectPermission)
       update <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe updatePermission)
       delete <- buildPermission -< (proxy, tableCache, source, table, tableFields, listToMaybe deletePermission)
       returnA -< RolePermInfo insert select update delete)
   |) alignedPermissions
  -- see [Inherited roles architecture for postgres read queries]
  (| foldlA' (\accumulatedRolePermMap (Role roleName (ParentRoles parentRoles)) -> do
       let metadataRoleSelectPermission = _permSel =<< M.lookup roleName accumulatedRolePermMap
       roleSelectPermission <-
         bindA -<
         case metadataRoleSelectPermission of
           -- There's an permission set for the role in the metadata, so we use that
           Just metadataRoleSelectPermission' -> pure $ Just metadataRoleSelectPermission'
           -- no permission found in the metadata, so we inherit permissions from the parent
           -- roles
           Nothing                            -> do
             parentRoleTablePerms <-
               for (toList parentRoles) $ \role ->
                 onNothing (M.lookup role accumulatedRolePermMap) $ throw500 $
                  -- this error will ideally never be thrown, but if it's thrown then
                  -- it's possible that the permissions for the role do exist, but it's
                  -- not yet built due to wrong ordering of the roles, check `orderRoles`
                  "buildTablePermissions: table role permissions for role: " <> role <<> " not found"
             pure $ combineSelectPermInfos <$> NE.nonEmpty (catMaybes $ _permSel <$> parentRoleTablePerms)
       let rolePermInfo = RolePermInfo Nothing roleSelectPermission Nothing Nothing
       returnA -< M.insert roleName rolePermInfo accumulatedRolePermMap)
   |) metadataRolePermissions (bool mempty orderedRoles $ EFInheritedRoles `elem` experimentalFeatures)
  -- build permissions for inherited roles only when inherited roles is enabled
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
  :: forall b a
   . (ToJSON (a b), BackendMetadata b, IsPerm a)
  => SourceName
  -> TableName b
  -> PermDef (a b)
  -> MetadataObject
mkPermissionMetadataObject source table permDef =
  let permType = permAccToType (permAccessor :: PermAccessor b (PermInfo a b))
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
  :: forall bknd a b c s arr
   . (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr, BackendMetadata bknd, ToJSON (c bknd), IsPerm c)
  => WriterA (Seq SchemaDependency) (ErrorA QErr arr) (a, s) b
  -> ( a
     , ((SourceName, TableName bknd, PermDef (c bknd), Proxy bknd), s)
     ) `arr` (Maybe b)
withPermission f = proc (e, ((source, table, permission, _proxy), s)) -> do
  let metadataObject = mkPermissionMetadataObject @bknd source table permission
      permType = permAccToType (permAccessor :: PermAccessor bknd (PermInfo c bknd))
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
   . ( ArrowChoice arr
     , ArrowWriter (Seq CollectedInfo) arr
     , Inc.ArrowCache m arr
     , Inc.Cacheable (a b)
     , Inc.Cacheable (Proxy b)
     , MonadError QErr m
     , BackendMetadata b
     , ToJSON (a b)
     , IsPerm a
     )
  => ( Proxy b
     , Inc.Dependency (TableCoreCache b)
     , SourceName
     , TableName b
     , FieldInfoMap (FieldInfo b)
     , Maybe (PermDef (a b))
     ) `arr` Maybe (PermInfo a b)
buildPermission = Inc.cache proc (proxy, tableCache, source, table, tableFields, maybePermission) -> do
  (| traverseA ( \permission ->
    (| withPermission (do
         bindErrorA -< when (_pdRole permission == adminRoleName) $
           throw400 ConstraintViolation "cannot define permission for admin role"
         (info, dependencies) <- liftEitherA <<< Inc.bindDepend -< runExceptT $
           runTableCoreCacheRT (buildPermInfo source table tableFields permission) (source, tableCache)
         tellA -< Seq.fromList dependencies
         returnA -< info)
     |) (source, table, permission, proxy))
   |) maybePermission
  >-> (\info -> join info >- returnA)
