{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema
  ( buildGQLContext,
  )
where

import Control.Concurrent.Extended (forConcurrentlyEIO)
import Control.Lens.Extended
import Data.Aeson.Ordered qualified as JO
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.Extended (duplicates)
import Data.Text.Extended
import Data.Text.NonEmpty qualified as NT
import Hasura.Base.Error
import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Types
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser
  ( Kind (..),
    Parser,
    Schema (..),
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Parser.Schema.Convert (convertToSchemaIntrospection)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Instances ()
import Hasura.GraphQL.Schema.Introspect
import Hasura.GraphQL.Schema.Postgres
import Hasura.GraphQL.Schema.Remote (buildRemoteParser)
import Hasura.GraphQL.Schema.RemoteRelationship
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.QueryTags
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Tag (HasTag)
import Hasura.Server.Types
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------
-- Building contexts

-- | Builds the full GraphQL context for a given query type.
--
-- A 'GQLContext' stores how an incoming request should be processed: how to
-- translate each incoming field of a request into a corresponding semantic
-- representation. There is a different one per 'Role', as each role might have
-- different permissions, and therefore not access to the same set of objects in
-- the schema.
--
-- This function takes all necessary information from the metadata, and the
-- 'GraphQLQueryType', and builds all relevant contexts: a hash map from
-- 'RoleName' to their 'GQLContext' and the "default" context for
-- unauthenticated users.
--
-- When building the schema for each role, we treat the remote schemas as
-- "second-class citizens" compared to sources; more specifically, we attempt to
-- detect whether the inclusion of a given remote schema would result in root
-- fields conflict, and only keep schemas that don't generate any. This results
-- in a partial schema being available to the users, and a better error message
-- than would arise from 'safeSelectionSet'.
buildGQLContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m
  ) =>
  ServerConfigCtx ->
  GraphQLQueryType ->
  SourceCache ->
  HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject) ->
  ActionCache ->
  AnnotatedCustomTypes ->
  m
    ( G.SchemaIntrospection,
      HashMap RoleName (RoleContext GQLContext),
      GQLContext,
      HashSet InconsistentMetadata
    )
buildGQLContext ServerConfigCtx {..} queryType sources allRemoteSchemas allActions customTypes = do
  let remoteSchemasRoles = concatMap (Map.keys . _rscPermissions . fst . snd) $ Map.toList allRemoteSchemas
      nonTableRoles =
        Set.insert adminRoleName $
          (allActionInfos ^.. folded . aiPermissions . to Map.keys . folded)
            <> Set.fromList (bool mempty remoteSchemasRoles $ _sccRemoteSchemaPermsCtx == RemoteSchemaPermsEnabled)
      allActionInfos = Map.elems allActions
      allTableRoles = Set.fromList $ getTableRoles =<< Map.elems sources
      allRoles = nonTableRoles <> allTableRoles
      defaultNC = _sccDefaultNamingConvention

  roleContexts <-
    -- Buld role contexts in parallel. We'd prefer deterministic parallelism
    -- but that isn't really acheivable (see mono #3829). NOTE: the admin role
    -- will still be a bottleneck here, even on huge_schema which has many
    -- roles.
    fmap Map.fromList $
      forConcurrentlyEIO 10 (Set.toList allRoles) $ \role ->
        (role,)
          <$> case queryType of
            QueryHasura ->
              buildRoleContext
                (_sccSQLGenCtx, queryType, _sccFunctionPermsCtx)
                sources
                allRemoteSchemas
                allActionInfos
                customTypes
                role
                _sccRemoteSchemaPermsCtx
                _sccExperimentalFeatures
                (bool StreamingSubscriptionsDisabled StreamingSubscriptionsEnabled $ EFStreamingSubscriptions `elem` _sccExperimentalFeatures)
                defaultNC
            QueryRelay ->
              (,mempty,G.SchemaIntrospection mempty)
                <$> buildRelayRoleContext
                  (_sccSQLGenCtx, queryType, _sccFunctionPermsCtx)
                  sources
                  allActionInfos
                  customTypes
                  role
                  _sccExperimentalFeatures
                  defaultNC

  adminIntrospection <-
    case Map.lookup adminRoleName roleContexts of
      Just (_context, _errors, introspection) -> pure introspection
      Nothing -> throw500 "buildGQLContext failed to build for the admin role"
  (unauthenticated, unauthenticatedRemotesErrors) <- unauthenticatedContext allRemoteSchemas _sccRemoteSchemaPermsCtx
  pure
    ( adminIntrospection,
      view _1 <$> roleContexts,
      unauthenticated,
      Set.unions $ unauthenticatedRemotesErrors : map (view _2) (Map.elems roleContexts)
    )

-- | Build the @QueryHasura@ context for a given role.
buildRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject) ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  RoleName ->
  RemoteSchemaPermsCtx ->
  Set.HashSet ExperimentalFeature ->
  StreamingSubscriptionsCtx ->
  Maybe NamingCase ->
  m
    ( RoleContext GQLContext,
      HashSet InconsistentMetadata,
      G.SchemaIntrospection
    )
buildRoleContext options sources remotes allActionInfos customTypes role remoteSchemaPermsCtx expFeatures streamingSubscriptionsCtx globalDefaultNC = do
  let ( SQLGenCtx stringifyNum dangerousBooleanCollapse optimizePermissionFilters,
        queryType,
        functionPermsCtx
        ) = options
      schemaOptions =
        SchemaOptions
          stringifyNum
          dangerousBooleanCollapse
          queryType
          functionPermsCtx
          remoteSchemaPermsCtx
          optimizePermissionFilters
      schemaContext =
        SchemaContext
          sources
          (remoteRelationshipField sources (fst <$> remotes))
  runMonadSchema schemaOptions schemaContext role $ do
    -- build all sources
    (sourcesQueryFields, sourcesMutationFrontendFields, sourcesMutationBackendFields, subscriptionFields) <-
      fmap mconcat $ traverse (buildBackendSource buildSource) $ toList sources
    -- build all remote schemas
    -- we only keep the ones that don't result in a name conflict
    (remoteSchemaFields, remoteSchemaErrors) <-
      buildAndValidateRemoteSchemas remotes sourcesQueryFields sourcesMutationBackendFields role remoteSchemaPermsCtx
    let remotesQueryFields = concatMap piQuery remoteSchemaFields
        remotesMutationFields = concat $ mapMaybe piMutation remoteSchemaFields
        remotesSubscriptionFields = concat $ mapMaybe piSubscription remoteSchemaFields

    mutationParserFrontend <-
      buildMutationParser remotesMutationFields allActionInfos customTypes sourcesMutationFrontendFields
    mutationParserBackend <-
      buildMutationParser remotesMutationFields allActionInfos customTypes sourcesMutationBackendFields
    subscriptionParser <-
      buildSubscriptionParser subscriptionFields allActionInfos customTypes remotesSubscriptionFields
    queryParserFrontend <-
      buildQueryParser sourcesQueryFields remotesQueryFields allActionInfos customTypes mutationParserFrontend subscriptionParser
    queryParserBackend <-
      buildQueryParser sourcesQueryFields remotesQueryFields allActionInfos customTypes mutationParserBackend subscriptionParser

    -- In order to catch errors early, we attempt to generate the data
    -- required for introspection, which ends up doing a few correctness
    -- checks in the GraphQL schema. Furthermore, we want to persist this
    -- information in the case of the admin role.
    introspectionSchema <- do
      result <-
        convertToSchemaIntrospection
          <$> buildIntrospectionSchema
            (P.parserType queryParserBackend)
            (P.parserType <$> mutationParserBackend)
            (P.parserType <$> subscriptionParser)
      pure $
        -- TODO(nicuveo): we treat the admin role differently in this function,
        -- which is a bit inelegant; we might want to refactor this function and
        -- split it into several steps, so that we can make a separate function for
        -- the admin role that reuses the common parts and avoid such tests.
        if role == adminRoleName
          then result
          else G.SchemaIntrospection mempty
    void $
      buildIntrospectionSchema
        (P.parserType queryParserFrontend)
        (P.parserType <$> mutationParserFrontend)
        (P.parserType <$> subscriptionParser)

    -- (since we're running this in parallel in caller, be strict)
    let !frontendContext =
          GQLContext
            (finalizeParser queryParserFrontend)
            (finalizeParser <$> mutationParserFrontend)
            (finalizeParser <$> subscriptionParser)
        !backendContext =
          GQLContext
            (finalizeParser queryParserBackend)
            (finalizeParser <$> mutationParserBackend)
            (finalizeParser <$> subscriptionParser)

    pure
      ( RoleContext frontendContext $ Just backendContext,
        remoteSchemaErrors,
        introspectionSchema
      )
  where
    buildSource ::
      forall b.
      BackendSchema b =>
      SourceInfo b ->
      ConcreteSchemaT
        m
        ( [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))],
          [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))],
          [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))],
          [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))]
        )
    buildSource sourceInfo@(SourceInfo _ tables functions _ _ sourceCustomization') =
      withSourceCustomization sourceCustomization (namingConventionSupport @b) globalDefaultNC do
        let validFunctions = takeValidFunctions functions
            validTables = takeValidTables tables
        mkTypename <- asks getter
        uncustomizedQueryFields <- buildQueryFields sourceInfo validTables validFunctions
        uncustomizedStreamSubscriptionFields <-
          case streamingSubscriptionsCtx of
            StreamingSubscriptionsEnabled -> buildTableStreamSubscriptionFields sourceInfo validTables
            StreamingSubscriptionsDisabled -> pure mempty
        (,,,)
          <$> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__query))
            (pure uncustomizedQueryFields)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__mutation_frontend))
            (buildMutationFields Frontend sourceInfo validTables validFunctions)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__mutation_backend))
            (buildMutationFields Backend sourceInfo validTables validFunctions)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__subscription))
            (pure $ uncustomizedStreamSubscriptionFields <> uncustomizedQueryFields)
      where
        sourceCustomization =
          if EFNamingConventions `elem` expFeatures
            then sourceCustomization'
            else sourceCustomization' {_scNamingConvention = Nothing}

buildRelayRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  RoleName ->
  Set.HashSet ExperimentalFeature ->
  Maybe NamingCase ->
  m (RoleContext GQLContext)
buildRelayRoleContext options sources allActionInfos customTypes role expFeatures globalDefaultNC = do
  let ( SQLGenCtx stringifyNum dangerousBooleanCollapse optimizePermissionFilters,
        queryType,
        functionPermsCtx
        ) = options
      schemaOptions =
        SchemaOptions
          stringifyNum
          dangerousBooleanCollapse
          queryType
          functionPermsCtx
          RemoteSchemaPermsDisabled
          optimizePermissionFilters
      -- TODO: At the time of writing this, remote schema queries are not supported in relay.
      -- When they are supported, we should get do what `buildRoleContext` does. Since, they
      -- are not supported yet, we use `mempty` below for `RemoteSchemaMap`.
      schemaContext =
        SchemaContext
          sources
          (remoteRelationshipField sources mempty)
  runMonadSchema schemaOptions schemaContext role do
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
      buildSubscriptionParser queryPGFields [] customTypes []
    queryParserFrontend <-
      queryWithIntrospectionHelper queryPGFields mutationParserFrontend subscriptionParser
    queryParserBackend <-
      queryWithIntrospectionHelper queryPGFields mutationParserBackend subscriptionParser

    -- In order to catch errors early, we attempt to generate the data
    -- required for introspection, which ends up doing a few correctness
    -- checks in the GraphQL schema.
    void $
      buildIntrospectionSchema
        (P.parserType queryParserBackend)
        (P.parserType <$> mutationParserBackend)
        (P.parserType <$> subscriptionParser)
    void $
      buildIntrospectionSchema
        (P.parserType queryParserFrontend)
        (P.parserType <$> mutationParserFrontend)
        (P.parserType <$> subscriptionParser)

    let frontendContext =
          GQLContext
            (finalizeParser queryParserFrontend)
            (finalizeParser <$> mutationParserFrontend)
            (finalizeParser <$> subscriptionParser)
        backendContext =
          GQLContext
            (finalizeParser queryParserBackend)
            (finalizeParser <$> mutationParserBackend)
            (finalizeParser <$> subscriptionParser)

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
    buildSource sourceInfo@(SourceInfo _ tables functions _ _ sourceCustomization') =
      withSourceCustomization sourceCustomization (namingConventionSupport @b) globalDefaultNC do
        let validFunctions = takeValidFunctions functions
            validTables = takeValidTables tables

        mkTypename <- asks getter
        (,,)
          <$> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__query))
            (buildRelayQueryFields sourceInfo validTables validFunctions)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__mutation_frontend))
            (buildMutationFields Frontend sourceInfo validTables validFunctions)
          <*> customizeFields
            sourceCustomization
            (mkTypename <> P.MkTypename (<> G.__mutation_backend))
            (buildMutationFields Backend sourceInfo validTables validFunctions)
      where
        sourceCustomization =
          if EFNamingConventions `elem` expFeatures
            then sourceCustomization'
            else sourceCustomization' {_scNamingConvention = Nothing}

-- | Builds the schema context for unauthenticated users.
--
-- This context is used whenever the user queries the engine with a role that is
-- unknown, and therefore not present in the context map. Before remote schema
-- permissions were introduced, remotes were considered to be a public entity,
-- and we therefore allowed an unknown role also to query the remotes. To
-- maintain backwards compatibility, we check if remote schema permissions are
-- enabled; remote schemas will only be available to unauthenticated users if
-- permissions aren't enabled.
unauthenticatedContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m
  ) =>
  HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject) ->
  RemoteSchemaPermsCtx ->
  m (GQLContext, HashSet InconsistentMetadata)
unauthenticatedContext allRemotes remoteSchemaPermsCtx = do
  -- Since remote schemas can theoretically join against tables, we need to give
  -- some fake data to 'runMonadSchema' in order to trick it into successfully
  -- building a restricted schema; namely, we erase all remote relationships
  -- from the remote schema contexts, meaning that all the information that is
  -- needed for sources is completely irrelevant and filled with default values.
  let fakeSchemaOptions =
        SchemaOptions
          LeaveNumbersAlone -- stringifyNum doesn't apply to remotes
          True -- booleanCollapse doesn't apply to remotes
          QueryHasura
          FunctionPermissionsInferred -- function permissions don't apply to remotes
          remoteSchemaPermsCtx
          False
      fakeSchemaContext =
        SchemaContext
          mempty
          ignoreRemoteRelationship
      -- chosen arbitrarily to be as improbable as possible
      fakeRole = mkRoleNameSafe [NT.nonEmptyTextQQ|MyNameIsOzymandiasKingOfKingsLookOnMyWorksYeMightyAndDespair|]
      -- we delete all references to remote joins
      alteredRemoteSchemas =
        allRemotes <&> first \context ->
          context {_rscRemoteRelationships = mempty}

  runMonadSchema fakeSchemaOptions fakeSchemaContext fakeRole do
    (queryFields, mutationFields, subscriptionFields, remoteErrors) <- case remoteSchemaPermsCtx of
      RemoteSchemaPermsEnabled ->
        -- Permissions are enabled, unauthenticated users have access to nothing.
        pure ([], [], [], mempty)
      RemoteSchemaPermsDisabled -> do
        -- Permissions are disabled, unauthenticated users have access to remote schemas.
        (remoteFields, remoteSchemaErrors) <-
          buildAndValidateRemoteSchemas alteredRemoteSchemas [] [] fakeRole remoteSchemaPermsCtx
        pure
          ( fmap (fmap RFRemote) <$> concatMap piQuery remoteFields,
            fmap (fmap RFRemote) <$> concat (mapMaybe piMutation remoteFields),
            fmap (fmap RFRemote) <$> concat (mapMaybe piSubscription remoteFields),
            remoteSchemaErrors
          )
    mutationParser <-
      whenMaybe (not $ null mutationFields) $
        P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFields
          <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)
    subscriptionParser <-
      whenMaybe (not $ null subscriptionFields) $
        P.safeSelectionSet subscriptionRoot (Just $ G.Description "subscription root") subscriptionFields
          <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)
    queryParser <- queryWithIntrospectionHelper queryFields mutationParser Nothing
    void $
      buildIntrospectionSchema
        (P.parserType queryParser)
        (P.parserType <$> mutationParser)
        (P.parserType <$> subscriptionParser)
    pure (GQLContext (finalizeParser queryParser) (finalizeParser <$> mutationParser) (finalizeParser <$> subscriptionParser), remoteErrors)

-------------------------------------------------------------------------------
-- Building parser fields

buildAndValidateRemoteSchemas ::
  forall m.
  ( MonadError QErr m,
    MonadIO m
  ) =>
  HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject) ->
  [FieldParser (P.ParseT Identity) (NamespacedField (QueryRootField UnpreparedValue))] ->
  [FieldParser (P.ParseT Identity) (NamespacedField (MutationRootField UnpreparedValue))] ->
  RoleName ->
  RemoteSchemaPermsCtx ->
  ConcreteSchemaT m ([RemoteSchemaParser (P.ParseT Identity)], HashSet InconsistentMetadata)
buildAndValidateRemoteSchemas remotes sourcesQueryFields sourcesMutationFields role remoteSchemaPermsCtx =
  runWriterT $ foldlM step [] (Map.elems remotes)
  where
    getFieldName = P.getName . fDefinition

    sourcesQueryFieldNames = getFieldName <$> sourcesQueryFields
    sourcesMutationFieldNames = getFieldName <$> sourcesMutationFields

    step validatedSchemas (remoteSchemaContext, metadataId) = do
      let previousSchemasQueryFieldNames = map getFieldName $ concatMap piQuery validatedSchemas
          previousSchemasMutationFieldNames = map getFieldName $ concat $ mapMaybe piMutation validatedSchemas
          reportInconsistency reason = tell $ Set.singleton $ InconsistentObject reason Nothing metadataId
      maybeParser <- lift $ buildRemoteSchemaParser remoteSchemaPermsCtx role remoteSchemaContext
      case maybeParser of
        Nothing -> pure validatedSchemas
        Just remoteSchemaParser -> do
          (_, inconsistencies) <- listen $ do
            let newSchemaQueryFieldNames = map getFieldName $ piQuery remoteSchemaParser
                newSchemaMutationFieldNames = foldMap (map getFieldName) $ piMutation remoteSchemaParser
            -- First we check for conflicts in query_root:
            --   - between this remote and the previous ones:
            for_
              (duplicates $ newSchemaQueryFieldNames <> previousSchemasQueryFieldNames)
              \name -> reportInconsistency $ "Duplicate remote field " <> squote name
            --   - between this remote and the sources:
            for_ (duplicates $ newSchemaQueryFieldNames <> sourcesQueryFieldNames) $
              \name -> reportInconsistency $ "Field cannot be overwritten by remote field " <> squote name
            -- Ditto, but for mutations - i.e. with mutation_root:
            unless (null newSchemaMutationFieldNames) do
              --   - between this remote and the previous ones:
              for_ (duplicates $ newSchemaMutationFieldNames <> previousSchemasMutationFieldNames) $
                \name -> reportInconsistency $ "Duplicate remote field " <> squote name
              --   - between this remote and the sources:
              for_ (duplicates $ newSchemaMutationFieldNames <> sourcesMutationFieldNames) $
                \name -> reportInconsistency $ "Field cannot be overwritten by remote field " <> squote name
          -- No need to check for conflicts between subscription fields, since
          -- remote subscriptions aren't supported yet.

          -- Only add this new remote to the list if there was no error
          pure $
            if Set.null inconsistencies
              then remoteSchemaParser : validatedSchemas
              else validatedSchemas

buildRemoteSchemaParser ::
  forall m.
  (MonadError QErr m, MonadIO m) =>
  RemoteSchemaPermsCtx ->
  RoleName ->
  RemoteSchemaCtx ->
  ConcreteSchemaT m (Maybe (RemoteSchemaParser (P.ParseT Identity)))
buildRemoteSchemaParser remoteSchemaPermsCtx roleName context = do
  let maybeIntrospection = getIntrospectionResult remoteSchemaPermsCtx roleName context
  for maybeIntrospection \introspection ->
    buildRemoteParser introspection (_rscRemoteRelationships context) (_rscInfo context)

buildQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableCache b ->
  FunctionCache b ->
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildQueryFields sourceInfo tables (takeExposedAs FEAQuery -> functions) = do
  roleName <- asks getter
  functionPermsCtx <- retrieve soFunctionPermsContext
  tableSelectExpParsers <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableIdentifierName <- getTableIdentifierName @b tableInfo
    mkRF $ buildTableQueryFields sourceInfo tableName tableInfo tableIdentifierName
  functionSelectExpParsers <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    guard $
      roleName == adminRoleName
        || roleName `Map.member` _fiPermissions functionInfo
        || functionPermsCtx == FunctionPermissionsInferred
    let targetTableName = _fiReturnType functionInfo
    lift $ mkRF $ buildFunctionQueryFields sourceInfo functionName functionInfo targetTableName
  pure $ concat $ tableSelectExpParsers <> catMaybes functionSelectExpParsers
  where
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig QDBR
    sourceName = _siName sourceInfo
    sourceConfig = _siConfiguration sourceInfo
    queryTagsConfig = _siQueryTagsConfig sourceInfo

buildTableStreamSubscriptionFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableCache b ->
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildTableStreamSubscriptionFields sourceInfo tables = do
  tableSelectExpParsers <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableGQLName <- getTableIdentifierName @b tableInfo
    mkRF $
      buildTableStreamingSubscriptionFields
        sourceInfo
        tableName
        tableInfo
        tableGQLName
  pure $ concat tableSelectExpParsers
  where
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig QDBR
    sourceName = _siName sourceInfo
    sourceConfig = _siConfiguration sourceInfo
    queryTagsConfig = _siQueryTagsConfig sourceInfo

buildRelayQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableCache b ->
  FunctionCache b ->
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildRelayQueryFields sourceInfo tables (takeExposedAs FEAQuery -> functions) = do
  tableConnectionFields <- for (Map.toList tables) \(tableName, tableInfo) -> runMaybeT do
    tableIdentifierName <- getTableIdentifierName @b tableInfo
    pkeyColumns <- hoistMaybe $ tableInfo ^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns
    lift $ mkRF $ buildTableRelayQueryFields sourceInfo tableName tableInfo tableIdentifierName pkeyColumns
  functionConnectionFields <- for (Map.toList functions) $ \(functionName, functionInfo) -> runMaybeT do
    let returnTableName = _fiReturnType functionInfo
    -- FIXME: only extract the TableInfo once to avoid redundant cache lookups
    returnTableInfo <- lift $ askTableInfo sourceInfo returnTableName
    pkeyColumns <- MaybeT $ (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns) <$> pure returnTableInfo
    lift $ mkRF $ buildFunctionRelayQueryFields sourceInfo functionName functionInfo returnTableName pkeyColumns
  pure $ concat $ catMaybes $ tableConnectionFields <> functionConnectionFields
  where
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig QDBR
    sourceName = _siName sourceInfo
    sourceConfig = _siConfiguration sourceInfo
    queryTagsConfig = _siQueryTagsConfig sourceInfo

buildMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  Scenario ->
  SourceInfo b ->
  TableCache b ->
  FunctionCache b ->
  m [P.FieldParser n (MutationRootField UnpreparedValue)]
buildMutationFields scenario sourceInfo tables (takeExposedAs FEAMutation -> functions) = do
  roleName <- asks getter
  tableMutations <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableIdentifierName <- getTableIdentifierName @b tableInfo
    inserts <-
      mkRF (MDBR . MDBInsert) $ buildTableInsertMutationFields scenario sourceInfo tableName tableInfo tableIdentifierName
    updates <-
      mkRF (MDBR . MDBUpdate) $ buildTableUpdateMutationFields scenario sourceInfo tableName tableInfo tableIdentifierName
    deletes <-
      mkRF (MDBR . MDBDelete) $ buildTableDeleteMutationFields scenario sourceInfo tableName tableInfo tableIdentifierName
    pure $ concat [inserts, updates, deletes]
  functionMutations <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    let targetTableName = _fiReturnType functionInfo
    -- A function exposed as mutation must have a function permission
    -- configured for the role. See Note [Function Permissions]
    guard $
      -- when function permissions are inferred, we don't expose the
      -- mutation functions for non-admin roles. See Note [Function Permissions]
      roleName == adminRoleName || roleName `Map.member` (_fiPermissions functionInfo)
    lift $ mkRF MDBR $ buildFunctionMutationFields sourceInfo functionName functionInfo targetTableName
  pure $ concat $ tableMutations <> catMaybes functionMutations
  where
    mkRF :: forall a db remote action raw. (a -> db b) -> m [FieldParser n a] -> m [FieldParser n (RootField db remote action raw)]
    mkRF = mkRootField sourceName sourceConfig queryTagsConfig
    sourceName = _siName sourceInfo
    sourceConfig = _siConfiguration sourceInfo
    queryTagsConfig = _siQueryTagsConfig sourceInfo

----------------------------------------------------------------
-- Building root parser from fields

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  [P.FieldParser n (NamespacedField (RemoteSchemaRootField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable))] ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  Maybe (Parser 'Output n (RootFieldMap (MutationRootField UnpreparedValue))) ->
  Maybe (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue))) ->
  m (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue)))
buildQueryParser sourceQueryFields remoteQueryFields allActions customTypes mutationParser subscriptionParser = do
  actionQueryFields <- concat <$> traverse (buildActionQueryFields customTypes) allActions
  let allQueryFields = sourceQueryFields <> fmap (fmap NotNamespaced) actionQueryFields <> fmap (fmap $ fmap RFRemote) remoteQueryFields
  queryWithIntrospectionHelper allQueryFields mutationParser subscriptionParser

-- | Builds a @Schema@ at query parsing time
parseBuildIntrospectionSchema ::
  MonadParse m =>
  P.Type 'Output ->
  Maybe (P.Type 'Output) ->
  Maybe (P.Type 'Output) ->
  m Schema
parseBuildIntrospectionSchema q m s = qerrAsMonadParse $ buildIntrospectionSchema q m s
  where
    qerrAsMonadParse :: MonadParse m => Except QErr a -> m a
    qerrAsMonadParse action =
      case runExcept action of
        Right a -> pure a
        Left QErr {..} -> withPath (++ qePath) $ parseErrorWith qeCode qeError

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
      placeholderField = NotNamespaced (RFRaw $ JO.String placeholderText) <$ P.selection_ G._no_queries_available (Just $ G.Description placeholderText) P.string
      fixedQueryFP = if null basicQueryFP then [placeholderField] else basicQueryFP
  basicQueryP <- queryRootFromFields fixedQueryFP
  let buildIntrospectionResponse printResponseFromSchema = do
        partialSchema <-
          parseBuildIntrospectionSchema
            (P.parserType basicQueryP)
            (P.parserType <$> mutationP)
            (P.parserType <$> subscriptionP)
        pure $ NotNamespaced $ RFRaw $ printResponseFromSchema partialSchema
      introspection = [schema, typeIntrospection] <&> (`P.bindField` buildIntrospectionResponse)
      {-# INLINE introspection #-}
      partialQueryFields = fixedQueryFP ++ introspection
  P.safeSelectionSet queryRoot Nothing partialQueryFields <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

queryRootFromFields ::
  forall n m.
  (MonadError QErr m, MonadParse n) =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  m (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue)))
queryRootFromFields fps =
  P.safeSelectionSet queryRoot Nothing fps <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

-- | Prepare the parser for subscriptions. Every postgres query field is
-- exposed as a subscription along with fields to get the status of
-- asynchronous actions.
buildSubscriptionParser ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (NamespacedField (QueryRootField UnpreparedValue))] ->
  [ActionInfo] ->
  AnnotatedCustomTypes ->
  [P.FieldParser n (NamespacedField (RemoteSchemaRootField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable))] ->
  m (Maybe (Parser 'Output n (RootFieldMap (QueryRootField UnpreparedValue))))
buildSubscriptionParser sourceSubscriptionFields allActions customTypes remoteSubscriptionFields = do
  actionSubscriptionFields <- fmap (fmap NotNamespaced) . concat <$> traverse (buildActionSubscriptionFields customTypes) allActions
  let subscriptionFields = sourceSubscriptionFields <> actionSubscriptionFields <> fmap (fmap $ fmap RFRemote) remoteSubscriptionFields
  whenMaybe (not $ null subscriptionFields) $
    P.safeSelectionSet subscriptionRoot Nothing subscriptionFields
      <&> fmap (flattenNamespaces . fmap typenameToNamespacedRawRF)

buildMutationParser ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (NamespacedField (RemoteSchemaRootField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable))] ->
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

-------------------------------------------------------------------------------
-- Local helpers

-- | Apply a source's customization options to a list of its fields.
customizeFields ::
  forall f n db remote action.
  (Functor f, MonadParse n) =>
  SourceCustomization ->
  P.MkTypename ->
  f [FieldParser n (RootField db remote action JO.Value)] ->
  f [FieldParser n (NamespacedField (RootField db remote action JO.Value))]
customizeFields SourceCustomization {..} =
  fmap . customizeNamespace (_rootfcNamespace =<< _scRootFields) (const typenameToRawRF)

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
subscriptionRoot = G._subscription_root

mutationRoot :: G.Name
mutationRoot = G._mutation_root

queryRoot :: G.Name
queryRoot = G._query_root

finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

type ConcreteSchemaT m a =
  P.SchemaT
    (P.ParseT Identity)
    ( ReaderT
        ( SchemaOptions,
          SchemaContext,
          RoleName,
          MkTypename,
          MkRootFieldName,
          CustomizeRemoteFieldName,
          NamingCase
        )
        m
    )
    a

runMonadSchema ::
  forall m a.
  Monad m =>
  SchemaOptions ->
  SchemaContext ->
  RoleName ->
  ConcreteSchemaT m a ->
  m a
runMonadSchema options context roleName m =
  flip runReaderT (options, context, roleName, mempty, mempty, mempty, HasuraCase) $
    P.runSchemaT m

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
