{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema
  ( buildGQLContext,
  )
where

import Control.Lens.Extended
import Control.Monad.Unique
import Data.Aeson.Ordered qualified as JO
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Maybe (fromJust)
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Types
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
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------
-- Building contexts

buildGQLContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadUnique m,
    HasServerConfigCtx m
  ) =>
  GraphQLQueryType ->
  SourceCache ->
  RemoteSchemaMap ->
  ActionCache ->
  NonObjectTypeMap ->
  m
    ( HashMap RoleName (RoleContext GQLContext),
      GQLContext
    )
buildGQLContext queryType sources allRemoteSchemas allActions nonObjectCustomTypes = do
  ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx _maintenanceMode _experimentalFeatures <- askServerConfigCtx
  let remoteSchemasRoles = concatMap (Map.keys . _rscPermissions . snd) $ Map.toList allRemoteSchemas
      nonTableRoles =
        Set.insert adminRoleName $
          (allActionInfos ^.. folded . aiPermissions . to Map.keys . folded)
            <> Set.fromList (bool mempty remoteSchemasRoles $ remoteSchemaPermsCtx == RemoteSchemaPermsEnabled)
      allActionInfos = Map.elems allActions
      allTableRoles = Set.fromList $ getTableRoles =<< Map.elems sources
      allRoles = nonTableRoles <> allTableRoles
  roleContexts <-
    Set.toMap allRoles & Map.traverseWithKey \role () ->
      case queryType of
        QueryHasura ->
          buildRoleContext
            (sqlGenCtx, queryType, functionPermsCtx)
            sources
            allRemoteSchemas
            allActionInfos
            nonObjectCustomTypes
            role
            remoteSchemaPermsCtx
        QueryRelay ->
          buildRelayRoleContext
            (sqlGenCtx, queryType, functionPermsCtx)
            sources
            allActionInfos
            nonObjectCustomTypes
            role
  unauthenticated <- unauthenticatedContext allRemoteSchemas remoteSchemaPermsCtx
  pure (roleContexts, unauthenticated)

buildRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m, MonadUnique m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  RemoteSchemaMap ->
  [ActionInfo] ->
  NonObjectTypeMap ->
  RoleName ->
  RemoteSchemaPermsCtx ->
  m (RoleContext GQLContext)
buildRoleContext
  (SQLGenCtx stringifyNum boolCollapse, queryType, functionPermsCtx)
  sources
  remotes
  allActionInfos
  nonObjectCustomTypes
  role
  remoteSchemaPermsCtx =
    do
      let roleQueryContext =
            QueryContext
              stringifyNum
              boolCollapse
              queryType
              functionPermsCtx
              remoteSchemaPermsCtx
      runMonadSchema role roleQueryContext sources remotes $ do
        roleBasedRemoteSchemas <- buildRoleBasedRemoteSchemaParser remoteSchemaPermsCtx role remotes
        sourceFieldsList <- traverse (buildBackendSource buildSource) $ toList sources
        let parsedIntrospections = snd <$> roleBasedRemoteSchemas
            queryRemotes = concatMap piQuery parsedIntrospections
            mutationRemotes = concat $ mapMaybe piMutation parsedIntrospections
            (queryFields, mutationFrontendFields, mutationBackendFields) = mconcat sourceFieldsList

        mutationParserFrontend <-
          buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes mutationFrontendFields
        mutationParserBackend <-
          buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes mutationBackendFields
        subscriptionParser <-
          buildSubscriptionParser queryFields allActionInfos
        queryParserFrontend <-
          buildQueryParser queryFields queryRemotes allActionInfos nonObjectCustomTypes mutationParserFrontend subscriptionParser
        queryParserBackend <-
          buildQueryParser queryFields queryRemotes allActionInfos nonObjectCustomTypes mutationParserBackend subscriptionParser

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
          ( [FieldParser (P.ParseT Identity) (QueryRootField UnpreparedValue)],
            [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)],
            [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig queryTagsConfig) = do
        let validFunctions = takeValidFunctions functions
            validTables = takeValidTables tables

        (,,)
          <$> buildQueryFields sourceName sourceConfig validTables validFunctions queryTagsConfig
          <*> buildMutationFields Frontend sourceName sourceConfig validTables validFunctions queryTagsConfig
          <*> buildMutationFields Backend sourceName sourceConfig validTables validFunctions queryTagsConfig

buildRelayRoleContext ::
  forall m.
  (MonadError QErr m, MonadIO m, MonadUnique m) =>
  (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) ->
  SourceCache ->
  [ActionInfo] ->
  NonObjectTypeMap ->
  RoleName ->
  m (RoleContext GQLContext)
buildRelayRoleContext
  (SQLGenCtx stringifyNum boolCollapse, queryType, functionPermsCtx)
  sources
  allActionInfos
  nonObjectCustomTypes
  role = do
    -- TODO: At the time of writing this, remote schema queries are not supported in relay.
    -- When they are supported, we should get do what `buildRoleContext` does. Since, they
    -- are not supported yet, we use `mempty` below for `RemoteRelationshipQueryContext`.
    let roleQueryContext =
          QueryContext
            stringifyNum
            boolCollapse
            queryType
            functionPermsCtx
            RemoteSchemaPermsDisabled
    runMonadSchema role roleQueryContext sources mempty do
      fieldsList <- traverse (buildBackendSource buildSource) $ toList sources

      -- Add node root field.
      -- FIXME: for now this is PG-only. This isn't a problem yet since for now only PG supports relay.
      -- To fix this, we'd need to first generalize `nodeField`.
      nodeField_ <- nodeField
      let (queryPGFields', mutationFrontendFields, mutationBackendFields) = mconcat fieldsList
          queryPGFields = nodeField_ : queryPGFields'

      -- Remote schema mutations aren't exposed in relay because many times it throws
      -- the conflicting definitions error between the relay types like `Node`, `PageInfo` etc
      mutationParserFrontend <-
        buildMutationParser mempty allActionInfos nonObjectCustomTypes mutationFrontendFields
      mutationParserBackend <-
        buildMutationParser mempty allActionInfos nonObjectCustomTypes mutationBackendFields
      subscriptionParser <-
        buildSubscriptionParser queryPGFields []
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
          ( [FieldParser (P.ParseT Identity) (QueryRootField UnpreparedValue)],
            [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)],
            [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig queryTagsConfig) = do
        let validFunctions = takeValidFunctions functions
            validTables = takeValidTables tables

        (,,)
          <$> buildRelayQueryFields sourceName sourceConfig validTables validFunctions queryTagsConfig
          <*> buildMutationFields Frontend sourceName sourceConfig validTables validFunctions queryTagsConfig
          <*> buildMutationFields Backend sourceName sourceConfig validTables validFunctions queryTagsConfig

-- The `unauthenticatedContext` is used when the user queries the graphql-engine
-- with a role that it's unaware of. Before remote schema permissions, remotes
-- were considered to be a public entity, hence, we allowed an unknown role also
-- to query the remotes. To maintain backwards compatibility, we check if the
-- remote schema permissions are enabled, and if it's we don't expose the remote
-- schema fields in the unauthenticatedContext, otherwise we expose them.
unauthenticatedContext ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadUnique m
  ) =>
  RemoteSchemaMap ->
  RemoteSchemaPermsCtx ->
  m GQLContext
unauthenticatedContext allRemotes remoteSchemaPermsCtx = do
  -- Since remote schemas can theoretically join against tables, we need to give
  -- some fake data to 'runMonadSchema' in order to trick it into successfully
  -- building a restricted schema; namely, we erase all remote relationships
  -- from the remote schema contexts, meaning that all the information that is
  -- needed for sources is completely irrelevant and filled with default values.
  let fakeQueryContext =
        QueryContext
          True -- stringifyNum doesn't apply to remotes
          True -- booleanCollapse doesn't apply to remotes
          QueryHasura
          FunctionPermissionsInferred -- function permissions don't apply to remotes
          remoteSchemaPermsCtx
      -- chosen arbitrarily to be as improbable as possible
      fakeRole = fromJust $ mkRoleName "MyNameIsOzymandiasKingOfKingsLookOnMyWorksYeMightyAndDespair"
      -- we delete all references to remote joins
      alteredRemoteSchemas = allRemotes <&> \remoteSchemaCtx -> remoteSchemaCtx {_rscRemoteRelationships = mempty}

  runMonadSchema fakeRole fakeQueryContext mempty mempty do
    (queryFields, mutationFields) <- case remoteSchemaPermsCtx of
      RemoteSchemaPermsEnabled ->
        -- Permissions are enabled, unauthenticated users have access to nothing.
        pure ([], [])
      RemoteSchemaPermsDisabled -> do
        -- Permissions are disabled, unauthenticated users have access to remote schemas.
        remoteFields <- buildRoleBasedRemoteSchemaParser remoteSchemaPermsCtx adminRoleName alteredRemoteSchemas
        pure
          ( fmap RFRemote <$> concatMap (piQuery . snd) remoteFields,
            fmap RFRemote <$> concat (mapMaybe (piMutation . snd) remoteFields)
          )
    mutationParser <-
      whenMaybe (not $ null mutationFields) $
        P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFields
          <&> fmap (fmap typenameToRawRF)
    queryParser <- queryWithIntrospectionHelper queryFields mutationParser Nothing
    pure $ GQLContext (finalizeParser queryParser) (finalizeParser <$> mutationParser)

----------------------------------------------------------------
-- Building parser fields

buildRoleBasedRemoteSchemaParser ::
  forall m.
  (MonadError QErr m, MonadIO m, MonadUnique m) =>
  RemoteSchemaPermsCtx ->
  RoleName ->
  RemoteSchemaMap ->
  ConcreteSchemaT m [(RemoteSchemaName, ParsedIntrospection)]
buildRoleBasedRemoteSchemaParser remoteSchemaPermsCtx roleName remoteSchemaCache = do
  let remoteSchemaIntroInfos = toList remoteSchemaCache
  remoteSchemaPerms <-
    for remoteSchemaIntroInfos \RemoteSchemaCtx {..} -> runMaybeT do
      introspection <-
        if
            | -- admin doesn't have a custom annotated introspection, defaulting to the original one
              roleName == adminRoleName ->
              pure _rscIntroOriginal
            | -- if permissions are disabled, the role map will be empty, defaulting to the original one
              remoteSchemaPermsCtx == RemoteSchemaPermsDisabled ->
              pure _rscIntroOriginal
            | -- otherwise, look the role up in the map; if we find nothing, then the role doesn't have access
              otherwise ->
              hoistMaybe $ Map.lookup roleName _rscPermissions
      (queryParsers, mutationParsers, subscriptionParsers) <- lift $ buildRemoteParser introspection _rscRemoteRelationships _rscInfo
      -- FIXME: why do we parse and return the subscription root? we don't allow them for remotes?
      pure (_rscName, ParsedIntrospection queryParsers mutationParsers subscriptionParsers)
  pure $ catMaybes remoteSchemaPerms

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
    for selectPerms $ buildTableQueryFields sourceName sourceConfig queryTagsConfig tableName tableInfo tableGQLName
  functionSelectExpParsers <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    guard $
      roleName == adminRoleName
        || roleName `Map.member` _fiPermissions functionInfo
        || functionPermsCtx == FunctionPermissionsInferred
    let targetTableName = _fiReturnType functionInfo
    targetTableInfo <- askTableInfo sourceName targetTableName
    selectPerms <- MaybeT $ tableSelectPermissions targetTableInfo
    lift $ buildFunctionQueryFields sourceName sourceConfig queryTagsConfig functionName functionInfo targetTableName selectPerms
  pure $ concat $ catMaybes $ tableSelectExpParsers <> functionSelectExpParsers

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
    lift $ buildTableRelayQueryFields sourceName sourceConfig queryTagsConfig tableName tableInfo tableGQLName pkeyColumns selectPerms
  functionConnectionFields <- for (Map.toList functions) $ \(functionName, functionInfo) -> runMaybeT do
    let returnTableName = _fiReturnType functionInfo
    -- FIXME: only extract the TableInfo once to avoid redundant cache lookups
    returnTableInfo <- lift $ askTableInfo sourceName returnTableName
    pkeyColumns <- MaybeT $ (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns) <$> pure returnTableInfo
    selectPerms <- MaybeT $ tableSelectPermissions returnTableInfo
    lift $ buildFunctionRelayQueryFields sourceName sourceConfig queryTagsConfig functionName functionInfo returnTableName pkeyColumns selectPerms
  pure $ concat $ catMaybes $ tableConnectionFields <> functionConnectionFields

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
          buildTableInsertMutationFields
            sourceName
            sourceConfig
            queryTagsConfig
            tableName
            tableInfo
            tableGQLName
            insertPerms
            _permSel
            _permUpd
      updates <- runMaybeT $ do
        guard $ isMutable viIsUpdatable viewInfo
        updatePerms <- hoistMaybe _permUpd
        lift $ buildTableUpdateMutationFields sourceName sourceConfig queryTagsConfig tableName tableInfo tableGQLName updatePerms _permSel
      deletes <- runMaybeT $ do
        guard $ isMutable viIsDeletable viewInfo
        deletePerms <- hoistMaybe _permDel
        lift $ buildTableDeleteMutationFields sourceName sourceConfig queryTagsConfig tableName tableInfo tableGQLName deletePerms _permSel
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
    lift $ buildFunctionMutationFields sourceName sourceConfig queryTagsConfig functionName functionInfo targetTableName selectPerms
  pure $ concat $ catMaybes $ tableMutations <> functionMutations

----------------------------------------------------------------
-- Building root parser from fields

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (QueryRootField UnpreparedValue)] ->
  [P.FieldParser n (RemoteField (SchemaRelationshipSelect UnpreparedValue))] ->
  [ActionInfo] ->
  NonObjectTypeMap ->
  Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))) ->
  Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))) ->
  m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
buildQueryParser pgQueryFields remoteFields allActions nonObjectCustomTypes mutationParser subscriptionParser = do
  actionQueryFields <- concat <$> traverse (buildActionQueryFields nonObjectCustomTypes) allActions
  let allQueryFields = pgQueryFields <> actionQueryFields <> map (fmap RFRemote) remoteFields
  queryWithIntrospectionHelper allQueryFields mutationParser subscriptionParser

queryWithIntrospectionHelper ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  [P.FieldParser n (QueryRootField UnpreparedValue)] ->
  Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))) ->
  Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))) ->
  m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP = do
  basicQueryP <- queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
  let directives = directivesInfo @n
  allBasicTypes <-
    collectTypes $
      catMaybes
        [ Just $ P.TypeDefinitionsWrapper $ P.parserType basicQueryP,
          Just $ P.TypeDefinitionsWrapper $ P.diArguments =<< directives,
          P.TypeDefinitionsWrapper . P.parserType <$> mutationP,
          P.TypeDefinitionsWrapper . P.parserType <$> subscriptionP
        ]
  allIntrospectionTypes <- collectTypes . P.parserType =<< queryRootFromFields emptyIntro
  let allTypes =
        Map.unions
          [ allBasicTypes,
            Map.filterWithKey (\name _info -> name /= queryRoot) allIntrospectionTypes
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
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  P.safeSelectionSet queryRoot Nothing partialQueryFields <&> fmap (fmap typenameToRawRF)

queryRootFromFields ::
  forall n m.
  (MonadError QErr m, MonadParse n) =>
  [P.FieldParser n (QueryRootField UnpreparedValue)] ->
  m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryRootFromFields fps =
  P.safeSelectionSet queryRoot Nothing fps <&> fmap (fmap typenameToRawRF)

emptyIntrospection ::
  forall m n.
  (MonadSchema n m, MonadError QErr m) =>
  m [P.FieldParser n (QueryRootField UnpreparedValue)]
emptyIntrospection = do
  emptyQueryP <- queryRootFromFields @n []
  introspectionTypes <- collectTypes (P.parserType emptyQueryP)
  let introspectionSchema =
        Schema
          { sDescription = Nothing,
            sTypes = introspectionTypes,
            sQueryType = P.parserType emptyQueryP,
            sMutationType = Nothing,
            sSubscriptionType = Nothing,
            sDirectives = mempty
          }
  return $ fmap (fmap RFRaw) [schema introspectionSchema, typeIntrospection introspectionSchema]

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
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (QueryRootField UnpreparedValue)] ->
  [ActionInfo] ->
  m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))))
buildSubscriptionParser queryFields allActions = do
  actionSubscriptionFields <- concat <$> traverse buildActionSubscriptionFields allActions
  let subscriptionFields = queryFields <> actionSubscriptionFields
  whenMaybe (not $ null subscriptionFields) $
    P.safeSelectionSet subscriptionRoot Nothing subscriptionFields
      <&> fmap (fmap typenameToRawRF)

buildMutationParser ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  [P.FieldParser n (RemoteField (SchemaRelationshipSelect UnpreparedValue))] ->
  [ActionInfo] ->
  NonObjectTypeMap ->
  [P.FieldParser n (MutationRootField UnpreparedValue)] ->
  m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))))
buildMutationParser allRemotes allActions nonObjectCustomTypes mutationFields = do
  actionParsers <- concat <$> traverse (buildActionMutationFields nonObjectCustomTypes) allActions
  let mutationFieldsParser =
        mutationFields
          <> actionParsers
          <> fmap (fmap RFRemote) allRemotes
  whenMaybe (not $ null mutationFieldsParser) $
    P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFieldsParser
      <&> fmap (fmap typenameToRawRF)

----------------------------------------------------------------
-- local helpers

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
          RemoteSchemaMap,
          QueryContext
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
  RemoteSchemaMap ->
  ConcreteSchemaT m a ->
  m a
runMonadSchema roleName queryContext sources remotes m =
  flip runReaderT (roleName, sources, remotes, queryContext) $ P.runSchemaT m

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

buildBackendSource ::
  (forall b. BackendSchema b => SourceInfo b -> r) ->
  AB.AnyBackend SourceInfo ->
  r
buildBackendSource f e = AB.dispatchAnyBackend @BackendSchema e f

typenameToRawRF ::
  P.ParsedSelection (RootField db remote action JO.Value) ->
  RootField db remote action JO.Value
typenameToRawRF = P.handleTypename $ RFRaw . JO.String . G.unName
