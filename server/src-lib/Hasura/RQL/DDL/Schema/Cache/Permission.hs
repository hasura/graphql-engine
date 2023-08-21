module Hasura.RQL.DDL.Schema.Cache.Permission
  ( buildTablePermissions,
    orderRoles,
    OrderedRoles,
    _unOrderedRoles,
    mkBooleanPermissionMap,
    resolveCheckPermission,
    buildLogicalModelPermissions,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Environment qualified as Env
import Data.Graph qualified as G
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.LogicalModel.API (LogicalModelName)
import Hasura.LogicalModel.Fields (LogicalModelFieldsLookupRT (..), LogicalModelFieldsRM (..), runLogicalModelFieldsLookup)
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..), WithLogicalModel (..))
import Hasura.LogicalModel.Types (LogicalModelField (..), LogicalModelLocation (..))
import Hasura.NativeQuery.Metadata (WithNativeQuery (..))
import Hasura.Prelude
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.IR.BoolExp (AnnRedactionExp (..))
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.Roles.Internal
  ( CheckPermission (..),
    CombineRolePermInfo (..),
    rolePermInfoToCombineRolePermInfo,
  )
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache

{- Note: [Inherited roles architecture for read queries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Schema generation
--------------------

Schema generation for inherited roles is similar to the schema
generation of non-inherited roles. In the case of inherited roles,
we combine the `SelectPermInfo`s of the
inherited role's role set and a new `SelectPermInfo` will be generated
which will be the select permission of the inherited role.

Two `SelPermInfo`s will be combined in the following manner:

1. Columns - The `SelPermInfo` contains a hashset of the columns that are
   accessible to the role. To combine two `SelPermInfo`s, every column of the
   hashset is coupled with the boolean expression (filter) of the `SelPermInfo`
   and a hash map of all the columns is created out of it, this hashmap is
   generated for the `SelPermInfo`s that are going to be combined. These hashmaps
   are then unioned and the values of these hashmaps are `OR`ed. When a column
   is accessible to all the select permissions then the nullability of the column
   is inferred from the DB column otherwise the column is explicitly marked as
   nullable to accomodate cell-value nullification.
2. Scalar computed fields - Scalar computed fields work the same as Columns (#1)
3. Filter / Boolean expression - The filters are combined using a `BoolOr`
4. Limit - Limits are combined by taking the maximum of the two limits
5. Allow Aggregation - Aggregation is allowed, if any of the permissions allow it.
6. Request Headers - Request headers are concatenated

2. SQL generation
-----------------

See note [SQL generation for inherited roles]

3. Introspection
----------------

The columns accessible to an inherited role are explicitly set to
nullable irrespective of the nullability of the DB column to accomodate
cell value nullification.
-}

mkBooleanPermissionMap :: (RoleName -> a) -> HashMap RoleName a -> OrderedRoles -> HashMap RoleName a
mkBooleanPermissionMap constructorFn metadataPermissions orderedRoles =
  foldl' combineBooleanPermission metadataPermissions $ _unOrderedRoles orderedRoles
  where
    combineBooleanPermission accumulatedPermMap (Role roleName (ParentRoles parentRoles)) =
      case HashMap.lookup roleName accumulatedPermMap of
        -- We check if a permission for the given role exists in the metadata, if it
        -- exists, we use that
        Just _ -> accumulatedPermMap
        -- 2. When the permission doesn't exist, we try to inherit the permission from its parent roles
        -- For boolean permissions, if any of the parent roles have a permission to access an entity,
        -- then the inherited role will also be able to access the entity.
        Nothing ->
          -- see Note [Roles Inheritance]
          let canInheritPermission = any ((`HashMap.member` accumulatedPermMap)) (toList parentRoles)
           in if canInheritPermission
                then HashMap.insert roleName (constructorFn roleName) accumulatedPermMap
                else accumulatedPermMap

-- | `OrderedRoles` is a data type to hold topologically sorted roles
--   according to each role's parent roles, see `orderRoles` for more details.
newtype OrderedRoles = OrderedRoles {_unOrderedRoles :: [Role]}
  deriving (Eq, Generic)

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
orderRoles ::
  (MonadError QErr m) =>
  [Role] ->
  m OrderedRoles
orderRoles allRoles = do
  -- inherited roles can be created from other inherited and non-inherited roles
  -- So, roles can be thought of as a graph where non-inherited roles don't have
  -- any outgoing edges and inherited roles as nodes with edges to its parent roles
  -- However, we can't allow cyclic roles since permissions built by a role is used
  -- by the dependent roles to build their permissions and if cyclic roles were to be
  -- allowed, the permissions building will be stuck in an infinite loop
  let graphNodesList = [(role, _rRoleName role, toList (_unParentRoles . _rParentRoles $ role)) | role <- allRoles]
  let orderedGraphNodes = G.stronglyConnComp graphNodesList -- topologically sort the nodes of the graph
      cyclicRoles = filter checkCycle orderedGraphNodes
  unless (null cyclicRoles) $ do
    -- we're appending the first element of the list at the end, so that the error message will
    -- contain the complete cycle of the roles
    let roleCycles = map (tshow . map (roleNameToTxt . _rRoleName) . appendFirstElementAtEnd . G.flattenSCC) cyclicRoles
    throw400 CyclicDependency $ "found cycle(s) in roles: " <> commaSeparated roleCycles
  let allOrderedRoles = G.flattenSCCs orderedGraphNodes
  pure $ OrderedRoles allOrderedRoles
  where
    checkCycle = \case
      G.AcyclicSCC _ -> False
      G.CyclicSCC _ -> True

    appendFirstElementAtEnd [] = []
    appendFirstElementAtEnd (x : xs) = (x : xs) ++ [x]

-- | `resolveCheckPermission` is a helper function which will convert the indermediate
--    type `CheckPermission` to its original type. It will record any metadata inconsistencies, if exists.
resolveCheckPermission ::
  (MonadWriter (Seq CollectItem) m) =>
  CheckPermission p ->
  RoleName ->
  InconsistentRoleEntity ->
  m (Maybe p)
resolveCheckPermission checkPermission roleName inconsistentEntity = do
  case checkPermission of
    CPInconsistent -> do
      let inconsistentObj =
            -- check `Conflicts while inheriting permissions` in `rfcs/inherited-roles-improvements.md`
            CollectInconsistentMetadata
              $ ConflictingInheritedPermission roleName inconsistentEntity
      tell $ Seq.singleton inconsistentObj
      pure Nothing
    CPDefined permissionDefn -> pure $ Just permissionDefn
    CPUndefined -> pure Nothing

resolveCheckTablePermission ::
  ( MonadWriter (Seq CollectItem) m,
    BackendMetadata b
  ) =>
  CheckPermission perm ->
  Maybe (RolePermInfo b) ->
  (RolePermInfo b -> Maybe perm) ->
  RoleName ->
  SourceName ->
  TableName b ->
  PermType ->
  m (Maybe perm)
resolveCheckTablePermission inheritedRolePermission accumulatedRolePermInfo permAcc roleName source table permType = do
  -- when for a given entity and role, a permission exists in the metadata, we override the metadata permission
  -- over the inherited permission
  let checkPermission = maybe inheritedRolePermission CPDefined (permAcc =<< accumulatedRolePermInfo)
      inconsistentRoleEntity = InconsistentTablePermission source (toTxt table) permType
  resolveCheckPermission checkPermission roleName inconsistentRoleEntity

data SelectPermissionSource b
  = SPSTable (TableName b)
  | SPSLogicalModel (LogicalModelLocation)

instance (Backend b) => ToTxt (SelectPermissionSource b) where
  toTxt = \case
    SPSTable tableName -> "table " <>> tableName
    SPSLogicalModel (LMLLogicalModel name) -> "logical model " <>> name
    SPSLogicalModel (LMLNativeQuery name) -> "native query " <>> name

resolveSelectPermission ::
  forall b m.
  ( MonadWriter (Seq CollectItem) m,
    BackendMetadata b
  ) =>
  SourceName ->
  SourceConfig b ->
  SelectPermissionSource b ->
  Role ->
  Maybe (CombinedSelPermInfo b) ->
  Int ->
  Maybe (SelPermInfo b) ->
  m (Maybe (SelPermInfo b))
resolveSelectPermission sourceName sourceConfig selectPermissionSource role@(Role roleName (ParentRoles parentRoles)) combinedParentRoleSelPermInfo selectPermissionsCount explicitSelectPermission = runMaybeT $ do
  -- Prefer the explicitly defined select permissions over the inherited ones, if they exist
  selPermInfo <- hoistMaybe $ explicitSelectPermission <|> computedInheritedSelPermInfo
  if supportsRedaction || all (== NoRedaction) (spiCols selPermInfo)
    then pure selPermInfo
    else do
      recordInconsistencyM Nothing errMetadataObj errReason
      mzero -- Return no select permissions (ie. no select access)
  where
    supportsRedaction :: Bool
    supportsRedaction = sourceSupportsColumnRedaction @b sourceConfig

    computedInheritedSelPermInfo :: Maybe (SelPermInfo b)
    computedInheritedSelPermInfo = combinedSelPermInfoToSelPermInfo selectPermissionsCount <$> combinedParentRoleSelPermInfo

    errMetadataObj :: MetadataObject
    errMetadataObj = MetadataObject (MOInheritedRole roleName) (toJSON role)

    errReason :: Text
    errReason =
      "The source "
        <> sourceName
        <<> " does not support inherited roles where the columns allowed are different in the column select permissions in the parent roles. This occurs for the column select permissions defined for the "
        <> toTxt selectPermissionSource
        <> " for the inherited role "
        <> roleName
        <<> ", which combines the roles "
        <> commaSeparated (dquote <$> toList parentRoles)

buildTablePermissions ::
  forall b m.
  ( MonadError QErr m,
    MonadWriter (Seq CollectItem) m,
    BackendMetadata b,
    GetAggregationPredicatesDeps b
  ) =>
  Env.Environment ->
  SourceName ->
  SourceConfig b ->
  TableCoreCache b ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  TablePermissionInputs b ->
  OrderedRoles ->
  HashMap LogicalModelName (LogicalModelMetadata b) ->
  m (RolePermInfoMap b)
buildTablePermissions env source sourceConfig tableCache tableName tableFields tablePermissions orderedRoles logicalModels = do
  let alignedPermissions = alignPermissions tablePermissions
      go accumulatedRolePermMap role@(Role roleName (ParentRoles parentRoles)) = do
        parentRolePermissions <-
          for (toList parentRoles) $ \parentRoleName ->
            onNothing (HashMap.lookup parentRoleName accumulatedRolePermMap)
              -- this error will ideally never be thrown, but if it's thrown then
              -- it's possible that the permissions for the role do exist, but it's
              -- not yet built due to wrong ordering of the roles, check `orderRoles`
              $ throw500 ("buildTablePermissions: table role permissions for role: " <> parentRoleName <<> " not found")

        let combinedParentRolePermInfo = mconcat $ fmap rolePermInfoToCombineRolePermInfo parentRolePermissions
            selectPermissionsCount = length $ filter (isJust . _permSel) parentRolePermissions
            accumulatedRolePermission = HashMap.lookup roleName accumulatedRolePermMap

        roleSelectPermission <- resolveSelectPermission source sourceConfig (SPSTable tableName) role (crpiSelPerm combinedParentRolePermInfo) selectPermissionsCount (_permSel =<< accumulatedRolePermission)
        roleInsertPermission <- resolveCheckTablePermission (crpiInsPerm combinedParentRolePermInfo) accumulatedRolePermission _permIns roleName source table PTInsert
        roleUpdatePermission <- resolveCheckTablePermission (crpiUpdPerm combinedParentRolePermInfo) accumulatedRolePermission _permUpd roleName source table PTUpdate
        roleDeletePermission <- resolveCheckTablePermission (crpiDelPerm combinedParentRolePermInfo) accumulatedRolePermission _permDel roleName source table PTDelete
        let rolePermInfo = RolePermInfo roleInsertPermission roleSelectPermission roleUpdatePermission roleDeletePermission
        pure $ HashMap.insert roleName rolePermInfo accumulatedRolePermMap

  metadataRolePermissions <-
    for alignedPermissions \(insertPermission, selectPermission, updatePermission, deletePermission) -> do
      insert <- buildPermission insertPermission
      select <- buildPermission selectPermission
      update <- buildPermission updatePermission
      delete <- buildPermission deletePermission
      pure $ RolePermInfo insert select update delete
  foldlM go metadataRolePermissions (_unOrderedRoles orderedRoles)
  where
    table = _tpiTable tablePermissions

    mkMap :: [PermDef b e] -> HashMap RoleName (PermDef b e)
    mkMap = mapFromL _pdRole

    alignPermissions TablePermissionInputs {..} =
      let insertsMap = (\a -> (Just a, Nothing, Nothing, Nothing)) <$> mkMap _tpiInsert
          selectsMap = (\a -> (Nothing, Just a, Nothing, Nothing)) <$> mkMap _tpiSelect
          updatesMap = (\a -> (Nothing, Nothing, Just a, Nothing)) <$> mkMap _tpiUpdate
          deletesMap = (\a -> (Nothing, Nothing, Nothing, Just a)) <$> mkMap _tpiDelete
          unionMap = HashMap.unionWith \(a, b, c, d) (a', b', c', d') -> (a <|> a', b <|> b', c <|> c', d <|> d')
       in insertsMap `unionMap` selectsMap `unionMap` updatesMap `unionMap` deletesMap

    buildPermission :: Maybe (PermDef b a) -> m (Maybe (PermInfo a b))
    buildPermission Nothing = pure Nothing
    buildPermission (Just permission) = do
      let metadataObject = mkPermissionMetadataObject permission
          permType = reflectPermDefPermission (_pdPermission permission)
          roleName = _pdRole permission
          schemaObject =
            SOSourceObj source
              $ AB.mkAnyBackend
              $ SOITableObj @b table
              $ TOPerm roleName permType
          addPermContext err = "in permission for role " <> roleName <<> ": " <> err
      withRecordInconsistencyM metadataObject $ modifyErr (addTableContext @b table . addPermContext) do
        when (_pdRole permission == adminRoleName)
          $ throw400 ConstraintViolation "cannot define permission for admin role"
        (info, dependencies) <-
          flip runTableCoreCacheRT tableCache
            . flip runReaderT sourceConfig
            $ runLogicalModelFieldsLookup _lmmFields logicalModels
            $ buildPermInfo
              env
              source
              table
              tableFields
              (_pdRole permission)
              (_pdPermission permission)
        recordDependenciesM metadataObject schemaObject dependencies
        pure info

    mkPermissionMetadataObject :: PermDef b a -> MetadataObject
    mkPermissionMetadataObject permDef =
      let permType = reflectPermDefPermission (_pdPermission permDef)
          objectId =
            MOSourceObjId source
              $ AB.mkAnyBackend
              $ SMOTableObj @b table
              $ MTOPerm (_pdRole permDef) permType
          definition = toJSON $ WithTable @b source table permDef
       in MetadataObject objectId definition

-- | Create the permission map for a native query based on the select
-- permissions given in metadata. Compare with 'buildTablePermissions'.
buildLogicalModelPermissions ::
  forall b m.
  ( MonadError QErr m,
    MonadWriter (Seq CollectItem) m,
    BackendMetadata b,
    GetAggregationPredicatesDeps b,
    LogicalModelFieldsRM b m
  ) =>
  SourceName ->
  SourceConfig b ->
  TableCoreCache b ->
  LogicalModelLocation ->
  InsOrdHashMap (Column b) (LogicalModelField b) ->
  InsOrdHashMap RoleName (SelPermDef b) ->
  OrderedRoles ->
  m (RolePermInfoMap b)
buildLogicalModelPermissions sourceName sourceConfig tableCache logicalModelLocation logicalModelFields selectPermissions orderedRoles = do
  let combineRolePermissions :: RolePermInfoMap b -> Role -> m (RolePermInfoMap b)
      combineRolePermissions acc role@(Role roleName (ParentRoles parentRoles)) = do
        -- This error will ideally never be thrown, but if it's thrown then
        -- it's possible that the permissions for the role do exist, but it's
        -- not yet built due to wrong ordering of the roles, check `orderRoles`.
        parentRolePermissions <-
          for (toList parentRoles) \parentRole ->
            HashMap.lookup parentRole acc
              `onNothing` throw500 ("buildTablePermissions: table role permissions for role: " <> parentRole <<> " not found")

        let -- What permissions are we inheriting?
            combinedParentRolePermInfo :: CombineRolePermInfo b
            combinedParentRolePermInfo = foldMap rolePermInfoToCombineRolePermInfo parentRolePermissions

            -- How many select permissions are we inheriting? See
            -- 'combinedSelPermInfoToSelPermInfo' for information on the
            -- optimisation this count enables.
            selectPermissionsCount :: Int
            selectPermissionsCount = length (mapMaybe _permSel parentRolePermissions)

            -- Does our specific role have any permissions?
            accumulatedRolePermission :: Maybe (RolePermInfo b)
            accumulatedRolePermission = HashMap.lookup roleName acc

        -- If we have a permission, we'll use it. Otherwise, we'll fall
        -- back to the inherited permission.
        roleSelectPermission <- resolveSelectPermission sourceName sourceConfig (SPSLogicalModel logicalModelLocation) role (crpiSelPerm combinedParentRolePermInfo) selectPermissionsCount (_permSel =<< accumulatedRolePermission)

        let rolePermInfo :: RolePermInfo b
            rolePermInfo = RolePermInfo Nothing roleSelectPermission Nothing Nothing

        pure (HashMap.insert roleName rolePermInfo acc)

  -- At the moment, we only support select permissions for logical models
  metadataRolePermissions <-
    for
      (InsOrdHashMap.toHashMap selectPermissions)
      (buildLogicalModelSelectPermission sourceName sourceConfig tableCache logicalModelLocation logicalModelFields)

  foldlM combineRolePermissions metadataRolePermissions (_unOrderedRoles orderedRoles)

buildLogicalModelSelectPermission ::
  forall b m.
  ( MonadError QErr m,
    MonadWriter (Seq CollectItem) m,
    BackendMetadata b,
    GetAggregationPredicatesDeps b,
    LogicalModelFieldsRM b m
  ) =>
  SourceName ->
  SourceConfig b ->
  TableCoreCache b ->
  LogicalModelLocation ->
  InsOrdHashMap (Column b) (LogicalModelField b) ->
  SelPermDef b ->
  m (RolePermInfo b)
buildLogicalModelSelectPermission sourceName sourceConfig tableCache logicalModelLocation logicalModelFields selectPermission = do
  let role :: RoleName
      role = _pdRole selectPermission

      -- An identifier for the object on we're going to need to depend to
      -- generate this permission.
      sourceObjId :: MetadataObjId
      sourceObjId =
        MOSourceObjId sourceName
          $ AB.mkAnyBackend
          $ SMOLogicalModelObj @b logicalModelLocation
          $ LMMOPerm role PTSelect

      -- The object we're going to use to track the dependency and any
      -- potential cache inconsistencies.
      -- we'll need to add one for each location a Logical Model can live in future (a Stored Procedure, etc)
      metadataObject :: MetadataObject
      metadataObject = case logicalModelLocation of
        LMLLogicalModel logicalModelName ->
          MetadataObject sourceObjId
            $ toJSON
              WithLogicalModel
                { _wlmSource = sourceName,
                  _wlmName = logicalModelName,
                  _wlmInfo = selectPermission
                }
        LMLNativeQuery nativeQueryName ->
          MetadataObject sourceObjId
            $ toJSON
              WithNativeQuery
                { _wnqSource = sourceName,
                  _wnqName = nativeQueryName,
                  _wnqInfo = selectPermission
                }

      -- An identifier for this permission within the metadata structure.
      schemaObject :: SchemaObjId
      schemaObject =
        SOSourceObj sourceName
          $ AB.mkAnyBackend
          $ SOILogicalModelObj @b logicalModelLocation
          $ LMOPerm role PTSelect

      modifyError :: ExceptT QErr m a -> ExceptT QErr m a
      modifyError = modifyErr \err ->
        addLogicalModelContext logicalModelLocation
          $ "in permission for role "
          <> role
          <<> ": "
          <> err

  logicalModels <- getLogicalModelFieldsLookup @b
  select <- withRecordInconsistencyM metadataObject $ modifyError do
    when (role == adminRoleName)
      $ throw400 ConstraintViolation "cannot define permission for admin role"

    (permissionInformation, dependencies) <-
      flip runTableCoreCacheRT tableCache
        . flip runReaderT sourceConfig
        $ flip runLogicalModelFieldsLookupRT logicalModels
        $ buildLogicalModelPermInfo sourceName logicalModelLocation logicalModelFields
        $ _pdPermission selectPermission

    recordDependenciesM metadataObject schemaObject dependencies
    pure permissionInformation

  pure (RolePermInfo Nothing select Nothing Nothing)
