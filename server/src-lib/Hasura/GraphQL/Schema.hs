{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema
  ( buildGQLContext,
  )
where

import Control.Lens.Extended
import Data.Aeson.Ordered qualified as JO
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.Extended (duplicates)
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Types
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser
  ( Kind (..),
    Parser,
    Schema (..),
    UnpreparedValue (..),
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Directives (directivesInfo)
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Introspect
import Hasura.GraphQL.Schema.Postgres
import Hasura.GraphQL.Schema.Remote (buildRemoteParser)
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Tag (HasTag)
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------
-- Building contexts

buildGQLContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    HasServerConfigCtx m
  ) =>
  GraphQLQueryType ->
  SourceCache ->
  RemoteSchemaCache ->
  ActionCache ->
  AnnotatedCustomTypes ->
  m
    ( HashMap RoleName (RoleContext GQLContext),
      GQLContext,
      Seq InconsistentMetadata
    )
buildGQLContext queryType sources allRemoteSchemas allActions customTypes = do
  ServerConfigCtx {..} <- askServerConfigCtx
  let SQLGenCtx {..} = _sccSQLGenCtx
  let remoteSchemasRoles = concatMap (Map.keys . _rscPermissions . fst . snd) $ Map.toList allRemoteSchemas

  let nonTableRoles =
        Set.insert adminRoleName $
          (allActionInfos ^.. folded . aiPermissions . to Map.keys . folded)
            <> Set.fromList (bool mempty remoteSchemasRoles $ _sccRemoteSchemaPermsCtx == RemoteSchemaPermsEnabled)
      allActionInfos = Map.elems allActions

      allTableRoles = Set.fromList $ getTableRoles =<< Map.elems sources
      adminRemoteRelationshipQueryCtx =
        allRemoteSchemas
          <&> ( \(RemoteSchemaCtx {..}, _metadataObj) ->
                  RemoteRelationshipQueryContext _rscIntroOriginal _rscParsed $ rsCustomizer _rscInfo
              )
      allRoles :: Set.HashSet RoleName
      allRoles = nonTableRoles <> allTableRoles
      -- The function permissions context doesn't actually matter because the
      -- admin will have access to the function anyway
      adminQueryContext =
        QueryContext
          stringifyNum
          dangerousBooleanCollapse
          queryType
          adminRemoteRelationshipQueryCtx
          FunctionPermissionsInferred
          optimizePermissionFilters

  -- build the admin DB-only context so that we can check against name clashes with remotes
  -- TODO: Is there a better way to check for conflicts without actually building the admin schema?
  adminHasuraDBContext <-
    buildFullestDBSchema adminQueryContext sources allActionInfos customTypes

  -- TODO factor out the common function; throw500 in both cases:
  queryFieldNames :: [G.Name] <-
    case P.parserType $ fst adminHasuraDBContext of
      -- It really ought to be this case; anything else is a programming error.
      P.TNamed _ (P.Definition _ _ (P.TIObject (P.ObjectInfo rootFields _interfaces))) ->
        pure $ fmap P.dName rootFields
      _ -> throw500 "We encountered an root query of unexpected GraphQL type.  It should be an object type."
  let mutationFieldNames :: [G.Name]
      mutationFieldNames =
        case P.parserType <$> snd adminHasuraDBContext of
          Just (P.TNamed _ def) ->
            case P.dInfo def of
              -- It really ought to be this case; anything else is a programming error.
              P.TIObject (P.ObjectInfo rootFields _interfaces) -> fmap P.dName rootFields
              _ -> []
          _ -> []

  -- This block of code checks that there are no conflicting root field names between remotes.
  let (remotes, remoteErrors) =
        runState (remoteSchemaFields queryFieldNames mutationFieldNames allRemoteSchemas) mempty

  let adminQueryRemotes = concatMap (piQuery . _rrscParsedIntrospection . snd) remotes
      adminMutationRemotes = concatMap (concat . piMutation . _rrscParsedIntrospection . snd) remotes

  roleContexts <-
    Set.toMap allRoles & Map.traverseWithKey \role () ->
      case queryType of
        QueryHasura ->
          buildRoleContext
            (_sccSQLGenCtx, queryType, _sccFunctionPermsCtx)
            sources
            allRemoteSchemas
            allActionInfos
            customTypes
            remotes
            role
            _sccRemoteSchemaPermsCtx
        QueryRelay ->
          buildRelayRoleContext
            (_sccSQLGenCtx, queryType, _sccFunctionPermsCtx)
            sources
            allActionInfos
            customTypes
            role

  unauthenticated <- unauthenticatedContext adminQueryRemotes adminMutationRemotes _sccRemoteSchemaPermsCtx
  pure (roleContexts, unauthenticated, remoteErrors)

customizeFields ::
  forall f n db remote action.
  (Functor f, MonadParse n) =>
  SourceCustomization ->
  P.MkTypename ->
  f [FieldParser n (RootField db remote action JO.Value)] ->
  f [FieldParser n (NamespacedField (RootField db remote action JO.Value))]
customizeFields SourceCustomization {..} =
  fmap . customizeNamespace (_rootfcNamespace =<< _scRootFields) (const typenameToRawRF)

buildRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  RemoteSchemaCache ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  [(RemoteSchemaName, RemoteRelationshipQueryContext)] ->
  RoleName ->
  RemoteSchemaPermsCtx ->
  m (RoleContext GQLContext)
buildRoleContext
  (SQLGenCtx stringifyNum dangerousBooleanCollapse optimizePermissionFilters, queryType, functionPermsCtx)
  sources
  allRemoteSchemas
  allActionInfos
  customTypes
  remotes
  role
  remoteSchemaPermsCtx =
    do
      roleBasedRemoteSchemas <-
        if
            | role == adminRoleName -> pure remotes
            | remoteSchemaPermsCtx == RemoteSchemaPermsEnabled -> buildRoleBasedRemoteSchemaParser role allRemoteSchemas
            -- when remote schema permissions are not enabled, then remote schemas
            -- are a public entity which is accesible to all the roles
            | otherwise -> pure remotes
      let parsedIntrospections = _rrscParsedIntrospection . snd <$> roleBasedRemoteSchemas
          queryRemotes = getQueryRemotes parsedIntrospections
          mutationRemotes = getMutationRemotes parsedIntrospections
          remoteRelationshipQueryContext = Map.fromList roleBasedRemoteSchemas
          roleQueryContext =
            QueryContext
              stringifyNum
              dangerousBooleanCollapse
              queryType
              remoteRelationshipQueryContext
              functionPermsCtx
              optimizePermissionFilters
      runMonadSchema role roleQueryContext sources $ do
        fieldsList <- traverse (buildBackendSource buildSource) $ toList sources
        let (queryFields, mutationFrontendFields, mutationBackendFields) = mconcat fieldsList

        mutationParserFrontend <-
          buildMutationParser mutationRemotes allActionInfos customTypes mutationFrontendFields
        mutationParserBackend <-
          buildMutationParser mutationRemotes allActionInfos customTypes mutationBackendFields
        subscriptionParser <-
          buildSubscriptionParser queryFields allActionInfos customTypes
        queryParserFrontend <-
          buildQueryParser queryFields queryRemotes allActionInfos customTypes mutationParserFrontend subscriptionParser
        queryParserBackend <-
          buildQueryParser queryFields queryRemotes allActionInfos customTypes mutationParserBackend subscriptionParser

        let frontendContext =
              GQLContext (finalizeParser queryParserFrontend) (finalizeParser <$> mutationParserFrontend)
            backendContext =
              GQLContext (finalizeParser queryParserBackend) (finalizeParser <$> mutationParserBackend)

        pure $ RoleContext frontendContext $ Just backendContext
    where
      getQueryRemotes ::
        [ParsedIntrospection] ->
        [P.FieldParser (P.ParseT Identity) (NamespacedField RemoteField)]
      getQueryRemotes = concatMap piQuery

      getMutationRemotes ::
        [ParsedIntrospection] ->
        [P.FieldParser (P.ParseT Identity) (NamespacedField RemoteField)]
      getMutationRemotes = concatMap (concat . piMutation)

      buildSource ::
        forall b.
        BackendSchema b =>
        SourceInfo b ->
        ConcreteSchemaT
          m
          ( [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))],
            [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))],
            [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig queryTagsConfig sourceCustomization) =
        withSourceCustomization sourceCustomization do
          let validFunctions = takeValidFunctions functions
              validTables = takeValidTables tables
          mkTypename <- asks getter
          (,,)
            <$> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_query")))
              (buildQueryFields sourceName sourceConfig validTables validFunctions queryTagsConfig)
            <*> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_mutation_frontend")))
              (buildMutationFields Frontend sourceName sourceConfig validTables validFunctions queryTagsConfig)
            <*> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_mutation_backend")))
              (buildMutationFields Backend sourceName sourceConfig validTables validFunctions queryTagsConfig)

buildRelayRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  RoleName ->
  m (RoleContext GQLContext)
buildRelayRoleContext
  (SQLGenCtx stringifyNum dangerousBooleanCollapse optimizePermissionFilters, queryType, functionPermsCtx)
  sources
  allActionInfos
  customTypes
  role = do
    -- TODO: At the time of writing this, remote schema queries are not supported in relay.
    -- When they are supported, we should get do what `buildRoleContext` does. Since, they
    -- are not supported yet, we use `mempty` below for `RemoteRelationshipQueryContext`.
    let roleQueryContext =
          QueryContext
            stringifyNum
            dangerousBooleanCollapse
            queryType
            mempty
            functionPermsCtx
            optimizePermissionFilters
    runMonadSchema role roleQueryContext sources do
      fieldsList <- traverse (buildBackendSource buildSource) $ toList sources

      -- Add node root field.
      -- FIXME: for now this is PG-only. This isn't a problem yet since for now only PG supports relay.
      -- To fix this, we'd need to first generalize `nodeField`.
      nodeField_ <- fmap NotNamespaced <$> nodeField
      let (queryPGFields', mutationFrontendFields, mutationBackendFields) = mconcat fieldsList
          queryPGFields = nodeField_ : queryPGFields'

      -- Remote schema mutations aren't exposed in relay because many times it throws
      -- the conflicting definitions error between the relay types like `Node`, `PageInfo` etc
      mutationParserFrontend <-
        buildMutationParser mempty allActionInfos customTypes mutationFrontendFields
      mutationParserBackend <-
        buildMutationParser mempty allActionInfos customTypes mutationBackendFields
      subscriptionParser <-
        buildSubscriptionParser queryPGFields [] customTypes
      queryParserFrontend <-
        queryWithIntrospectionHelper queryPGFields mutationParserFrontend subscriptionParser
      queryParserBackend <-
        queryWithIntrospectionHelper queryPGFields mutationParserBackend subscriptionParser

      let frontendContext =
            GQLContext (finalizeParser queryParserFrontend) (finalizeParser <$> mutationParserFrontend)
          backendContext =
            GQLContext (finalizeParser queryParserBackend) (finalizeParser <$> mutationParserBackend)

      pure $ RoleContext frontendContext $ Just backendContext
    where
      buildSource ::
        forall b.
        BackendSchema b =>
        SourceInfo b ->
        ConcreteSchemaT
          m
          ( [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))],
            [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))],
            [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig queryTagsConfig sourceCustomization) =
        withSourceCustomization sourceCustomization do
          let validFunctions = takeValidFunctions functions
              validTables = takeValidTables tables

          mkTypename <- asks getter
          (,,)
            <$> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_query")))
              (buildRelayQueryFields sourceName sourceConfig validTables validFunctions queryTagsConfig)
            <*> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_mutation_frontend")))
              (buildMutationFields Frontend sourceName sourceConfig validTables validFunctions queryTagsConfig)
            <*> customizeFields
              sourceCustomization
              (mkTypename <> P.MkTypename (<> $$(G.litName "_mutation_backend")))
              (buildMutationFields Backend sourceName sourceConfig validTables validFunctions queryTagsConfig)

buildFullestDBSchema ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  QueryContext ->
  SourceCache ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  m
    ( Parser 'Output (P.ParseT Identity) (RootFieldMap (QueryRootField UnpreparedValue)),
      Maybe (Parser 'Output (P.ParseT Identity) (RootFieldMap (MutationRootField UnpreparedValue)))
    )
buildFullestDBSchema queryContext sources allActionInfos customTypes =
  runMonadSchema adminRoleName queryContext sources do
    fieldsList <- traverse (buildBackendSource buildSource) $ toList sources
    let (queryFields, mutationFrontendFields) = mconcat fieldsList

    mutationParserFrontend <-
      -- NOTE: we omit remotes here on purpose since we're trying to check name
      -- clashes with remotes:
      buildMutationParser mempty allActionInfos customTypes mutationFrontendFields
    subscriptionParser <-
      buildSubscriptionParser queryFields allActionInfos customTypes
    queryParserFrontend <-
      buildQueryParser queryFields mempty allActionInfos customTypes mutationParserFrontend subscriptionParser

    pure (queryParserFrontend, mutationParserFrontend)
  where
    buildSource ::
      forall b.
      BackendSchema b =>
      SourceInfo b ->
      ConcreteSchemaT
        m
        ( [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))],
          [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))]
        )
    buildSource (SourceInfo sourceName tables functions sourceConfig queryTagsConfig sourceCustomization) =
      withSourceCustomization sourceCustomization do
        let validFunctions = takeValidFunctions functions
            validTables = takeValidTables tables

        mkTypename <- asks getter
        (,)
          <$> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> $$(G.litName "_query")))
            (buildQueryFields sourceName sourceConfig validTables validFunctions queryTagsConfig)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> $$(G.litName "_mutation_frontend")))
            (buildMutationFields Frontend sourceName sourceConfig validTables validFunctions queryTagsConfig)

-- The `unauthenticatedContext` is used when the user queries the graphql-engine
-- with a role that it's unaware of. Before remote schema permissions, remotes
-- were considered to be a public entity, hence, we allowed an unknown role also
-- to query the remotes. To maintain backwards compatibility, we check if the
-- remote schema permissions are enabled, and if it's we don't expose the remote
-- schema fields in the unauthenticatedContext, otherwise we expose them.
unauthenticatedContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m
  ) =>
  [P.FieldParser (P.ParseT Identity) (NamespacedField RemoteField)] ->
  [P.FieldParser (P.ParseT Identity) (NamespacedField RemoteField)] ->
  RemoteSchemaPermsCtx ->
  m GQLContext
unauthenticatedContext adminQueryRemotes adminMutationRemotes remoteSchemaPermsCtx = P.runSchemaT $ do
  let isRemoteSchemaPermsEnabled = remoteSchemaPermsCtx == RemoteSchemaPermsEnabled
      queryFields = bool (fmap (fmap $ fmap RFRemote) adminQueryRemotes) [] isRemoteSchemaPermsEnabled
      mutationFields = bool (fmap (fmap $ fmap RFRemote) adminMutationRemotes) [] isRemoteSchemaPermsEnabled
  mutationParser <-
    whenMaybe (not $ null mutationFields) $
      P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFields
        <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)
  queryParser <- queryWithIntrospectionHelper queryFields mutationParser Nothing
  pure $ GQLContext (finalizeParser queryParser) (finalizeParser <$> mutationParser)

----------------------------------------------------------------
-- Building parser fields

buildRoleBasedRemoteSchemaParser ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  RoleName ->
  RemoteSchemaCache ->
  m [(RemoteSchemaName, RemoteRelationshipQueryContext)]
buildRoleBasedRemoteSchemaParser roleName remoteSchemaCache = do
  let remoteSchemaIntroInfos = map fst $ toList remoteSchemaCache
  remoteSchemaPerms <-
    for remoteSchemaIntroInfos $ \RemoteSchemaCtx {..} ->
      for (Map.lookup roleName _rscPermissions) $ \introspectRes -> do
        let customizer = rsCustomizer _rscInfo
        parsedIntrospection <- buildRemoteParser introspectRes _rscInfo
        return (_rscName, RemoteRelationshipQueryContext introspectRes parsedIntrospection customizer)
  return $ catMaybes remoteSchemaPerms

-- checks that there are no conflicting root field names between remotes and
-- hasura fields
remoteSchemaFields ::
  forall m.
  MonadState (Seq InconsistentMetadata) m =>
  [G.Name] ->
  [G.Name] ->
  HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject) ->
  m [(RemoteSchemaName, RemoteRelationshipQueryContext)]
remoteSchemaFields queryFieldNames mutationFieldNames allRemoteSchemas = do
  foldlM go [] $ Map.toList allRemoteSchemas
  where
    go ::
      [(RemoteSchemaName, RemoteRelationshipQueryContext)] ->
      (RemoteSchemaName, (RemoteSchemaCtx, MetadataObject)) ->
      m [(RemoteSchemaName, RemoteRelationshipQueryContext)]
    go okSchemas (newSchemaName, (RemoteSchemaCtx {..}, newMetadataObject)) = do
      let (queryOld, mutationOld) =
            unzip $ fmap ((\case ParsedIntrospection q m _ -> (q, m)) . _rrscParsedIntrospection . snd) okSchemas
      let ParsedIntrospection queryNew mutationNew _subscriptionNew =
            _rscParsed
      checkedDuplicates <- runExceptT do
        -- First we check for conflicts in query_root
        -- Check for conflicts between remotes
        for_ (duplicates (fmap (P.getName . fDefinition) (queryNew ++ concat queryOld))) $
          \name -> throwError (newMetadataObject, "Duplicate remote field " <> squote name)
        -- Check for conflicts between this remote and the tables
        for_ (duplicates (fmap (P.getName . fDefinition) queryNew ++ queryFieldNames)) $
          \name -> throwError (newMetadataObject, "Field cannot be overwritten by remote field " <> squote name)
        -- Ditto, but for mutations - i.e. with mutation_root
        onJust mutationNew \ms -> do
          -- Check for conflicts between remotes
          for_ (duplicates (fmap (P.getName . fDefinition) (ms ++ concat (catMaybes mutationOld)))) $
            \name -> throwError (newMetadataObject, "Duplicate remote field " <> squote name)
          -- Check for conflicts between this remote and the tables
          for_ (duplicates (fmap (P.getName . fDefinition) ms ++ mutationFieldNames)) $
            \name -> throwError (newMetadataObject, "Field cannot be overwritten by remote field " <> squote name)
      -- No need to check for conflicts with other subscriptions, since remote subscriptions are not supported

      -- Only add remote if no errors found
      case checkedDuplicates of
        Left (meta, reason) -> do
          withRecordInconsistency' reason meta
          return $ okSchemas
        Right () ->
          return $ (newSchemaName, RemoteRelationshipQueryContext _rscIntroOriginal _rscParsed $ rsCustomizer _rscInfo) : okSchemas
    -- variant of 'withRecordInconsistency' that works with 'MonadState' rather than 'ArrowWriter'
    withRecordInconsistency' reason metadata = modify' (InconsistentObject reason Nothing metadata Seq.:<|)

buildQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  TableCache b ->
  FunctionCache b ->
  Maybe QueryTagsConfig ->
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildQueryFields sourceName sourceConfig tables (takeExposedAs FEAQuery -> functions) queryTagsConfig = do
  roleName <- askRoleName
  functionPermsCtx <- asks $ qcFunctionPermsContext . getter
  tableSelectExpParsers <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableGQLName <- getTableGQLName @b tableInfo
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    selectPerms <- tableSelectPermissions tableInfo
    for selectPerms $ mkRF . buildTableQueryFields sourceName tableName tableInfo tableGQLName
  functionSelectExpParsers <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    guard $
      roleName == adminRoleName
        || roleName `Map.member` _fiPermissions functionInfo
        || functionPermsCtx == FunctionPermissionsInferred
    let targetTableName = _fiReturnType functionInfo
    targetTableInfo <- askTableInfo sourceName targetTableName
    selectPerms <- MaybeT $ tableSelectPermissions targetTableInfo
    lift $ mkRF $ buildFunctionQueryFields sourceName functionName functionInfo targetTableName selectPerms
  pure $ concat $ catMaybes $ tableSelectExpParsers <> functionSelectExpParsers
  where
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig QDBR

buildRelayQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  TableCache b ->
  FunctionCache b ->
  Maybe QueryTagsConfig ->
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildRelayQueryFields sourceName sourceConfig tables (takeExposedAs FEAQuery -> functions) queryTagsConfig = do
  tableConnectionFields <- for (Map.toList tables) \(tableName, tableInfo) -> runMaybeT do
    tableGQLName <- getTableGQLName @b tableInfo
    pkeyColumns <- hoistMaybe $ tableInfo ^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    selectPerms <- MaybeT $ tableSelectPermissions tableInfo
    lift $ mkRF $ buildTableRelayQueryFields sourceName tableName tableInfo tableGQLName pkeyColumns selectPerms
  functionConnectionFields <- for (Map.toList functions) $ \(functionName, functionInfo) -> runMaybeT do
    let returnTableName = _fiReturnType functionInfo
    -- FIXME: only extract the TableInfo once to avoid redundant cache lookups
    returnTableInfo <- lift $ askTableInfo sourceName returnTableName
    pkeyColumns <- MaybeT $ (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns) <$> pure returnTableInfo
    selectPerms <- MaybeT $ tableSelectPermissions returnTableInfo
    lift $ mkRF $ buildFunctionRelayQueryFields sourceName functionName functionInfo returnTableName pkeyColumns selectPerms
  pure $ concat $ catMaybes $ tableConnectionFields <> functionConnectionFields
  where
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig QDBR

buildMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  Scenario ->
  SourceName ->
  SourceConfig b ->
  TableCache b ->
  FunctionCache b ->
  Maybe QueryTagsConfig ->
  m [P.FieldParser n (MutationRootField UnpreparedValue)]
buildMutationFields scenario sourceName sourceConfig tables (takeExposedAs FEAMutation -> functions) queryTagsConfig = do
  roleName <- askRoleName
  tableMutations <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableGQLName <- getTableGQLName @b tableInfo
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    tablePerms <- tablePermissions tableInfo
    for tablePerms \RolePermInfo {..} -> do
      let viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
      inserts <- runMaybeT $ do
        guard $ isMutable viIsInsertable viewInfo
        insertPerms <- hoistMaybe $ do
          -- If we're in a frontend scenario, we should not include backend_only inserts
          insertPerms <- _permIns
          if scenario == Frontend && ipiBackendOnly insertPerms
            then Nothing
            else Just insertPerms
        lift $
          mkRF (MDBR . MDBInsert) $ buildTableInsertMutationFields sourceName tableName tableInfo tableGQLName insertPerms _permSel _permUpd
      updates <- runMaybeT $ do
        guard $ isMutable viIsUpdatable viewInfo
        updatePerms <- hoistMaybe _permUpd
        lift $ mkRF (MDBR . MDBUpdate) $ buildTableUpdateMutationFields @b sourceName tableName tableInfo tableGQLName updatePerms _permSel
      deletes <- runMaybeT $ do
        guard $ isMutable viIsDeletable viewInfo
        deletePerms <- hoistMaybe _permDel
        lift $ mkRF (MDBR . MDBDelete) $ buildTableDeleteMutationFields sourceName tableName tableInfo tableGQLName deletePerms _permSel
      pure $ concat $ catMaybes [inserts, updates, deletes]
  functionMutations <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    let targetTableName = _fiReturnType functionInfo
    targetTableInfo <- askTableInfo sourceName targetTableName
    selectPerms <- MaybeT $ tableSelectPermissions targetTableInfo
    -- A function exposed as mutation must have a function permission
    -- configured for the role. See Note [Function Permissions]
    guard $
      -- when function permissions are inferred, we don't expose the
      -- mutation functions for non-admin roles. See Note [Function Permissions]
      roleName == adminRoleName || roleName `Map.member` (_fiPermissions functionInfo)
    lift $ mkRF MDBR $ buildFunctionMutationFields sourceName functionName functionInfo targetTableName selectPerms
  pure $ concat $ catMaybes $ tableMutations <> functionMutations
  where
    mkRF :: forall a db remote action raw. (a -> db b) -> m [FieldParser n a] -> m [FieldParser n (RootField db remote action raw)]
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig

----------------------------------------------------------------
-- Building root parser from fields

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser ::
  forall m n r.
  ( MonadSchema n m,
    MonadTableInfo r m,
    MonadRole r m,
    Has QueryContext r,
    Has P.MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r
  ) =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  [P.FieldParser n (NamespacedField RemoteField)] ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  Maybe (Parser 'Output n (RootFieldMap (MutationRootField UnpreparedValue))) ->
  Maybe (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue))) ->
  m (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue)))
buildQueryParser pgQueryFields remoteFields allActions customTypes mutationParser subscriptionParser = do
  actionQueryFields <- concat <$> traverse (buildActionQueryFields customTypes) allActions
  let allQueryFields = pgQueryFields <> fmap (fmap NotNamespaced) actionQueryFields <> fmap (fmap $ fmap RFRemote) remoteFields
  queryWithIntrospectionHelper allQueryFields mutationParser subscriptionParser

queryWithIntrospectionHelper ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  Maybe (Parser 'Output n (RootFieldMap (MutationRootField UnpreparedValue))) ->
  Maybe (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue))) ->
  m (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue)))
queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP = do
  let -- Per the GraphQL spec:
      --  * "The query root operation type must be provided and must be an Object type." (§3.2.1)
      --  * "An Object type must define one or more fields." (§3.6, type validation)
      -- Those two requirements cannot both be met when a service is mutations-only, and does not
      -- provide any query. In such a case, to meet both of those, we introduce a placeholder query
      -- in the schema.
      placeholderText = "There are no queries available to the current role. Either there are no sources or remote schemas configured, or the current role doesn't have the required permissions."
      placeholderField = NotNamespaced (RFRaw $ JO.String placeholderText) <$ P.selection_ $$(G.litName "no_queries_available") (Just $ G.Description placeholderText) P.string
      fixedQueryFP = if null basicQueryFP then [placeholderField] else basicQueryFP
  basicQueryP <- queryRootFromFields fixedQueryFP
  let directives = directivesInfo @n
  -- We extract the types from the ordinary GraphQL schema (excluding introspection)
  allBasicTypes <-
    collectTypes $
      catMaybes
        [ Just $ P.TypeDefinitionsWrapper $ P.parserType basicQueryP,
          Just $ P.TypeDefinitionsWrapper $ P.diArguments =<< directives,
          P.TypeDefinitionsWrapper . P.parserType <$> mutationP,
          P.TypeDefinitionsWrapper . P.parserType <$> subscriptionP
        ]
  let introspection = [schema, typeIntrospection]
      {-# INLINE introspection #-}

  -- TODO: it may be worth looking at whether we can stop collecting
  -- introspection types monadically.  They are independent of the user schema;
  -- the types here are always the same and specified by the GraphQL spec

  -- Pull all the introspection types out (__Type, __Schema, etc)
  allIntrospectionTypes <- collectTypes (map fDefinition introspection)

  let allTypes =
        Map.unions
          [ allBasicTypes,
            Map.filterWithKey (\name _info -> name /= P.getName queryRoot) allIntrospectionTypes
          ]
      partialSchema =
        Schema
          { sDescription = Nothing,
            sTypes = allTypes,
            sQueryType = P.parserType basicQueryP,
            sMutationType = P.parserType <$> mutationP,
            sSubscriptionType = P.parserType <$> subscriptionP,
            sDirectives = directives
          }
  let buildIntrospectionResponse fromSchema = NotNamespaced $ RFRaw $ fromSchema partialSchema
      partialQueryFields =
        fixedQueryFP ++ (fmap buildIntrospectionResponse <$> introspection)
  P.safeSelectionSet queryRoot Nothing partialQueryFields <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

queryRootFromFields ::
  forall n m.
  (MonadError QErr m, MonadParse n) =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  m (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue)))
queryRootFromFields fps =
  P.safeSelectionSet queryRoot Nothing fps <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

collectTypes ::
  forall m a.
  (MonadError QErr m, P.HasTypeDefinitions a) =>
  a ->
  m (HashMap G.Name (P.Definition P.SomeTypeInfo))
collectTypes x =
  P.collectTypeDefinitions x
    `onLeft` \(P.ConflictingDefinitions (type1, origin1) (_type2, origins)) ->
      throw500 $
        "Found conflicting definitions for " <> P.getName type1 <<> ".  The definition at " <> origin1
          <<> " differs from the the definition at " <> commaSeparated origins
          <<> "."

-- | Prepare the parser for subscriptions. Every postgres query field is
-- exposed as a subscription along with fields to get the status of
-- asynchronous actions.
buildSubscriptionParser ::
  forall m n r.
  ( MonadSchema n m,
    MonadTableInfo r m,
    MonadRole r m,
    Has QueryContext r,
    Has P.MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r
  ) =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  m (Maybe (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue))))
buildSubscriptionParser queryFields allActions customTypes = do
  actionSubscriptionFields <- fmap (fmap NotNamespaced) . concat <$> traverse (buildActionSubscriptionFields customTypes) allActions
  let subscriptionFields = queryFields <> actionSubscriptionFields
  whenMaybe (not $ null subscriptionFields) $
    P.safeSelectionSet subscriptionRoot Nothing subscriptionFields
      <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

buildMutationParser ::
  forall m n r.
  ( MonadSchema n m,
    MonadTableInfo r m,
    MonadRole r m,
    Has QueryContext r,
    Has P.MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r
  ) =>
  [P.FieldParser n (NamespacedField RemoteField)] ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  [P.FieldParser n (NamespacedField (MutationRootField UnpreparedValue))] ->
  m (Maybe (Parser 'Output n (RootFieldMap (MutationRootField UnpreparedValue))))
buildMutationParser allRemotes allActions customTypes mutationFields = do
  actionParsers <- concat <$> traverse (buildActionMutationFields customTypes) allActions
  let mutationFieldsParser =
        mutationFields
          <> (fmap NotNamespaced <$> actionParsers)
          <> (fmap (fmap RFRemote) <$> allRemotes)
  whenMaybe (not $ null mutationFieldsParser) $
    P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFieldsParser
      <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

----------------------------------------------------------------
-- local helpers

-- | All the 'BackendSchema' methods produce something of the form @m
-- [FieldParser n a]@, where @a@ is something specific to what is being parsed
-- by the given method.
--
-- In order to build the complete schema these must be
-- homogenised and be annotated with query-tag data, which this function makes
-- easy.
mkRootField ::
  forall b m n a db remote action raw.
  (HasTag b, Functor m, Functor n) =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  (a -> db b) ->
  m [FieldParser n a] ->
  m [FieldParser n (RootField db remote action raw)]
mkRootField sourceName sourceConfig queryTagsConfig inj =
  fmap
    ( map
        ( fmap
            ( RFDB sourceName
                . AB.mkAnyBackend @b
                . SourceConfigWith sourceConfig queryTagsConfig
                . inj
            )
        )
    )

takeExposedAs :: FunctionExposedAs -> FunctionCache b -> FunctionCache b
takeExposedAs x = Map.filter ((== x) . _fiExposedAs)

subscriptionRoot :: G.Name
subscriptionRoot = $$(G.litName "subscription_root")

mutationRoot :: G.Name
mutationRoot = $$(G.litName "mutation_root")

queryRoot :: G.Name
queryRoot = $$(G.litName "query_root")

finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

type ConcreteSchemaT m a =
  P.SchemaT
    (P.ParseT Identity)
    ( ReaderT
        ( RoleName,
          SourceCache,
          QueryContext,
          P.MkTypename,
          MkRootFieldName,
          CustomizeRemoteFieldName
        )
        m
    )
    a

runMonadSchema ::
  forall m a.
  Monad m =>
  RoleName ->
  QueryContext ->
  SourceCache ->
  ConcreteSchemaT m a ->
  m a
runMonadSchema roleName queryContext pgSources m =
  P.runSchemaT m `runReaderT` (roleName, pgSources, queryContext, mempty, mempty, mempty)

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

type RemoteSchemaCache = HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)

buildBackendSource ::
  (forall b. BackendSchema b => SourceInfo b -> r) ->
  AB.AnyBackend SourceInfo ->
  r
buildBackendSource f e = AB.dispatchAnyBackend @BackendSchema e f

typenameToNamespacedRawRF ::
  P.ParsedSelection (NamespacedField (RootField db remote action JO.Value)) ->
  NamespacedField (RootField db remote action JO.Value)
typenameToNamespacedRawRF = P.handleTypename $ NotNamespaced . RFRaw . JO.String . toTxt

typenameToRawRF ::
  P.ParsedSelection (RootField db remote action JO.Value) ->
  RootField db remote action JO.Value
typenameToRawRF = P.handleTypename $ RFRaw . JO.String . toTxt
