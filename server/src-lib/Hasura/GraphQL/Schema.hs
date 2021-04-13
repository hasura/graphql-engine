{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}
module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as Set
import qualified Language.GraphQL.Draft.Syntax         as G

import           Control.Arrow.Extended
import           Control.Lens.Extended
import           Control.Monad.Unique
import           Data.Has
import           Data.List.Extended                    (duplicates)

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.SQL.AnyBackend                 as AB

import           Data.Text.Extended
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Parser                 (Kind (..), Parser, Schema (..),
                                                        UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Instances       ()
import           Hasura.GraphQL.Schema.Introspect
import           Hasura.GraphQL.Schema.Postgres
import           Hasura.GraphQL.Schema.Remote          (buildRemoteParser)
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.Session


----------------------------------------------------------------
-- Building contexts

buildGQLContext
  :: forall arr m
   . ( ArrowChoice arr
     , ArrowWriter (Seq InconsistentMetadata) arr
     , ArrowKleisli m arr
     , MonadError QErr m
     , MonadIO m
     , MonadUnique m
     , HasServerConfigCtx m
     )
  => ( GraphQLQueryType
     , SourceCache
     , RemoteSchemaCache
     , ActionCache
     , NonObjectTypeMap
     )
     `arr`
     ( HashMap RoleName (RoleContext GQLContext)
     , GQLContext
     )
buildGQLContext =
  proc (queryType, sources, allRemoteSchemas, allActions, nonObjectCustomTypes) -> do
    ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx@(SQLGenCtx stringifyNum boolCollapse) _maintenanceMode _experimentalFeatures <-
      bindA -< askServerConfigCtx
    let remoteSchemasRoles = concatMap (Map.keys . _rscPermissions . fst . snd) $ Map.toList allRemoteSchemas

    let nonTableRoles =
          Set.insert adminRoleName $
          (allActionInfos ^.. folded.aiPermissions.to Map.keys.folded)
          <> Set.fromList (bool mempty remoteSchemasRoles $ remoteSchemaPermsCtx == RemoteSchemaPermsEnabled)
        allActionInfos = Map.elems allActions

        allTableRoles = Set.fromList $ getTableRoles =<< Map.elems sources
        adminRemoteRelationshipQueryCtx =
          allRemoteSchemas
          <&> (\(remoteSchemaCtx, _metadataObj) ->
                 (_rscIntro remoteSchemaCtx, _rscParsed remoteSchemaCtx))
        allRoles :: Set.HashSet RoleName
        allRoles = nonTableRoles <> allTableRoles
        -- The function permissions context doesn't actually matter because the
        -- admin will have access to the function anyway
        adminQueryContext = QueryContext
          stringifyNum
          boolCollapse
          queryType
          adminRemoteRelationshipQueryCtx
          FunctionPermissionsInferred

    -- build the admin DB-only context so that we can check against name clashes with remotes
    -- TODO: Is there a better way to check for conflicts without actually building the admin schema?
    adminHasuraDBContext <- bindA -<
      buildFullestDBSchema adminQueryContext sources allActionInfos nonObjectCustomTypes

    -- TODO factor out the common function; throw500 in both cases:
    queryFieldNames :: [G.Name] <- bindA -<
      case P.discardNullability $ P.parserType $ fst adminHasuraDBContext of
        -- It really ought to be this case; anything else is a programming error.
        P.TNamed (P.Definition _ _ _ (P.TIObject (P.ObjectInfo rootFields _interfaces))) ->
          pure $ fmap P.dName rootFields
        _ -> throw500 "We encountered an root query of unexpected GraphQL type.  It should be an object type."
    let mutationFieldNames :: [G.Name]
        mutationFieldNames =
          case P.discardNullability . P.parserType <$> snd adminHasuraDBContext of
            Just (P.TNamed def) ->
              case P.dInfo def of
                -- It really ought to be this case; anything else is a programming error.
                P.TIObject (P.ObjectInfo rootFields _interfaces) -> fmap P.dName rootFields
                _                                                -> []
            _ -> []

    -- This block of code checks that there are no conflicting root field names between remotes.
    remotes <- remoteSchemaFields -< (queryFieldNames, mutationFieldNames, allRemoteSchemas)

    let adminQueryRemotes = concatMap (piQuery . snd . snd) remotes
        adminMutationRemotes = concatMap (concat . piMutation . snd . snd) remotes

    roleContexts <- bindA -<
      ( Set.toMap allRoles & Map.traverseWithKey \role () ->
          case queryType of
            QueryHasura ->
              buildRoleContext (sqlGenCtx, queryType, functionPermsCtx) sources allRemoteSchemas allActionInfos
              nonObjectCustomTypes remotes role remoteSchemaPermsCtx
            QueryRelay ->
              buildRelayRoleContext (sqlGenCtx, queryType, functionPermsCtx) sources allActionInfos
              nonObjectCustomTypes role
      )
    unauthenticated <- bindA -< unauthenticatedContext adminQueryRemotes adminMutationRemotes remoteSchemaPermsCtx
    returnA -< (roleContexts, unauthenticated)

buildRoleContext
  :: forall m. (MonadError QErr m, MonadIO m, MonadUnique m)
  => (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) -> SourceCache -> RemoteSchemaCache
  -> [ActionInfo] -> NonObjectTypeMap
  -> [( RemoteSchemaName , (IntrospectionResult, ParsedIntrospection))]
  -> RoleName
  -> RemoteSchemaPermsCtx
  -> m (RoleContext GQLContext)
buildRoleContext (SQLGenCtx stringifyNum boolCollapse, queryType, functionPermsCtx) sources
  allRemoteSchemas allActionInfos nonObjectCustomTypes remotes role remoteSchemaPermsCtx = do

  roleBasedRemoteSchemas <-
    if | role == adminRoleName                            -> pure remotes
       | remoteSchemaPermsCtx == RemoteSchemaPermsEnabled -> buildRoleBasedRemoteSchemaParser role allRemoteSchemas
       -- when remote schema permissions are not enabled, then remote schemas
       -- are a public entity which is accesible to all the roles
       | otherwise                                        -> pure remotes

  let queryRemotes    = getQueryRemotes $ snd . snd <$> roleBasedRemoteSchemas
      mutationRemotes = getMutationRemotes $ snd . snd <$> roleBasedRemoteSchemas
      remoteRelationshipQueryContext = Map.fromList roleBasedRemoteSchemas
      roleQueryContext = QueryContext
        stringifyNum
        boolCollapse
        queryType
        remoteRelationshipQueryContext
        functionPermsCtx
      buildSource :: forall b. BackendSchema b => SourceInfo b ->
        m ( [FieldParser (P.ParseT Identity) (QueryRootField    UnpreparedValue)]
          , [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          , [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig) = do
        let validFunctions = takeValidFunctions functions
            validTables    = takeValidTables tables
            xNodesAgg      = nodesAggExtension sourceConfig
            xRelay         = relayExtension    sourceConfig
        runMonadSchema role roleQueryContext sources (BackendExtension @b xRelay xNodesAgg) $
          (,,)
            <$> buildQueryFields sourceName sourceConfig validTables validFunctions
            <*> buildMutationFields Frontend sourceName sourceConfig validTables validFunctions
            <*> buildMutationFields Backend  sourceName sourceConfig validTables validFunctions

  fieldsList <- traverse (buildBackendSource buildSource) $ toList sources
  let (queryFields, mutationFrontendFields, mutationBackendFields) = mconcat fieldsList

  -- It's okay to run the rest of this while assuming that the backend is 'Postgres:
  -- the only remaining parsers are for actions, that are postgres specific, or for
  -- remotes, which are backend-agnostic.
  -- In the long term, all backend-specific processing should be moved to `buildSource`, and this
  -- block should be running in the schema for a `None` backend.
  runMonadSchema role roleQueryContext sources (BackendExtension @'Postgres (Just ()) (Just ())) $ do
    mutationParserFrontend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes mutationFrontendFields

    mutationParserBackend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes mutationBackendFields

    subscriptionParser <- buildSubscriptionParser queryFields allActionInfos

    queryParserFrontend <- buildQueryParser queryFields queryRemotes
      allActionInfos nonObjectCustomTypes mutationParserFrontend subscriptionParser
    queryParserBackend <- buildQueryParser queryFields queryRemotes
      allActionInfos nonObjectCustomTypes mutationParserBackend subscriptionParser

    let frontendContext = GQLContext (finalizeParser queryParserFrontend)
                          (finalizeParser <$> mutationParserFrontend)
    let backendContext = GQLContext (finalizeParser queryParserBackend)
                         (finalizeParser <$> mutationParserBackend)
    pure $ RoleContext frontendContext $ Just backendContext

  where
    getQueryRemotes
      :: [ParsedIntrospection]
      -> [P.FieldParser (P.ParseT Identity) RemoteField]
    getQueryRemotes = concatMap piQuery

    getMutationRemotes
      :: [ParsedIntrospection]
      -> [P.FieldParser (P.ParseT Identity) RemoteField]
    getMutationRemotes = concatMap (concat . piMutation)

buildRelayRoleContext
  :: forall m. (MonadError QErr m, MonadIO m, MonadUnique m)
  => (SQLGenCtx, GraphQLQueryType, FunctionPermissionsCtx) -> SourceCache -> [ActionInfo] -> NonObjectTypeMap
  -> RoleName
  -> m (RoleContext GQLContext)
buildRelayRoleContext (SQLGenCtx stringifyNum boolCollapse, queryType, functionPermsCtx) sources
  allActionInfos nonObjectCustomTypes role = do
  -- TODO: At the time of writing this, remote schema queries are not supported in relay.
  -- When they are supported, we should get do what `buildRoleContext` does. Since, they
  -- are not supported yet, we use `mempty` below for `RemoteRelationshipQueryContext`.
  let roleQueryContext = QueryContext
        stringifyNum
        boolCollapse
        queryType
        mempty
        functionPermsCtx
      buildSource :: forall b. BackendSchema b => SourceInfo b ->
        m ( [FieldParser (P.ParseT Identity) (QueryRootField    UnpreparedValue)]
          , [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          , [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig) = do
        let validFunctions = takeValidFunctions functions
            validTables    = takeValidTables tables
            xNodesAgg      = nodesAggExtension sourceConfig
            xRelay         = relayExtension    sourceConfig
        runMonadSchema role roleQueryContext sources (BackendExtension @b xRelay xNodesAgg) $
          (,,)
          <$> buildRelayQueryFields sourceName sourceConfig validTables validFunctions
          <*> buildMutationFields Frontend sourceName sourceConfig validTables validFunctions
          <*> buildMutationFields Backend sourceName sourceConfig validTables validFunctions

  fieldsList <- traverse (buildBackendSource buildSource) $ toList sources

  -- It's okay to run the rest of this while assuming that the backend is 'Postgres:
  -- the only remaining parsers are for actions, that are postgres specific, or for
  -- remotes, which are backend-agnostic.
  -- In the long term, all backend-specific processing should be moved to `buildSource`, and this
  -- block should be running in the schema for a `None` backend.
  runMonadSchema role roleQueryContext sources (BackendExtension @'Postgres (Just ()) (Just ())) $ do
    -- Add node root field.
    -- FIXME: for now this is PG-only. This isn't a problem yet since for now only PG supports relay.
    -- To fix this, we'd need to first generalize `nodeField`.
    nodeField_ <- nodeField
    let (queryPGFields', mutationFrontendFields, mutationBackendFields) = mconcat fieldsList
        queryPGFields = nodeField_:queryPGFields'

    -- Remote schema mutations aren't exposed in relay because many times it throws
    -- the conflicting definitions error between the relay types like `Node`, `PageInfo` etc
    mutationParserFrontend <-
      buildMutationParser mempty allActionInfos nonObjectCustomTypes mutationFrontendFields

    mutationParserBackend <-
      buildMutationParser mempty allActionInfos nonObjectCustomTypes mutationBackendFields

    subscriptionParser <- P.safeSelectionSet subscriptionRoot Nothing queryPGFields
                             <&> fmap (fmap (P.handleTypename (RFRaw . J.String. G.unName)))
    queryParserFrontend <- queryWithIntrospectionHelper queryPGFields
      mutationParserFrontend subscriptionParser
    queryParserBackend <- queryWithIntrospectionHelper queryPGFields
      mutationParserBackend subscriptionParser

    let frontendContext = GQLContext (finalizeParser queryParserFrontend)
                          (finalizeParser <$> mutationParserFrontend)
    let backendContext = GQLContext (finalizeParser queryParserBackend)
                         (finalizeParser <$> mutationParserBackend)
    pure $ RoleContext frontendContext $ Just backendContext

buildFullestDBSchema
  :: forall m. (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> SourceCache -> [ActionInfo] -> NonObjectTypeMap
  -> m ( Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
       , Maybe (Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
       )
buildFullestDBSchema queryContext sources allActionInfos nonObjectCustomTypes = do
  let buildSource :: forall b. BackendSchema b => SourceInfo b ->
        m ( [FieldParser (P.ParseT Identity) (QueryRootField    UnpreparedValue)]
          , [FieldParser (P.ParseT Identity) (MutationRootField UnpreparedValue)]
          )
      buildSource (SourceInfo sourceName tables functions sourceConfig) = do
        let validFunctions = takeValidFunctions functions
            validTables    = takeValidTables tables
            xNodesAgg      = nodesAggExtension sourceConfig
            xRelay         = relayExtension    sourceConfig
        runMonadSchema adminRoleName queryContext sources (BackendExtension @b xRelay xNodesAgg) $
          (,)
            <$> buildQueryFields sourceName sourceConfig validTables validFunctions
            <*> buildMutationFields Frontend sourceName sourceConfig validTables validFunctions

  fieldsList <- traverse (buildBackendSource buildSource) $ toList sources
  let (queryFields, mutationFrontendFields) = mconcat fieldsList

  -- It's okay to run the rest of this while assuming that the backend is 'Postgres:
  -- the only remaining parsers are for actions, that are postgres specific, or for
  -- remotes, which are backend-agnostic.
  -- In the long term, all backend-specific processing should be moved to `buildSource`, and this
  -- block should be running in the schema for a `None` backend.
  runMonadSchema adminRoleName queryContext sources (BackendExtension @'Postgres (Just ()) (Just ())) $ do
    mutationParserFrontend <-
      -- NOTE: we omit remotes here on purpose since we're trying to check name
      -- clashes with remotes:
      buildMutationParser mempty allActionInfos nonObjectCustomTypes mutationFrontendFields

    subscriptionParser <- buildSubscriptionParser queryFields allActionInfos

    queryParserFrontend <- buildQueryParser queryFields mempty
      allActionInfos nonObjectCustomTypes mutationParserFrontend subscriptionParser

    pure (queryParserFrontend, mutationParserFrontend)

-- The `unauthenticatedContext` is used when the user queries the graphql-engine
-- with a role that it's unaware of. Before remote schema permissions, remotes
-- were considered to be a public entity, hence, we allowed an unknown role also
-- to query the remotes. To maintain backwards compatibility, we check if the
-- remote schema permissions are enabled, and if it's we don't expose the remote
-- schema fields in the unauthenticatedContext, otherwise we expose them.
unauthenticatedContext
  :: forall m
   . ( MonadError QErr m
     , MonadIO m
     , MonadUnique m
     )
  => [P.FieldParser (P.ParseT Identity) RemoteField]
  -> [P.FieldParser (P.ParseT Identity) RemoteField]
  -> RemoteSchemaPermsCtx
  -> m GQLContext
unauthenticatedContext adminQueryRemotes adminMutationRemotes remoteSchemaPermsCtx = P.runSchemaT $ do
  let isRemoteSchemaPermsEnabled = remoteSchemaPermsCtx == RemoteSchemaPermsEnabled
      queryFields = bool (fmap (fmap RFRemote) adminQueryRemotes) [] isRemoteSchemaPermsEnabled
      mutationFields = bool (fmap (fmap RFRemote) adminMutationRemotes) [] isRemoteSchemaPermsEnabled
  mutationParser <-
    if null adminMutationRemotes
    then pure Nothing
    else P.safeSelectionSet mutationRoot Nothing mutationFields
         <&> Just . fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))
  subscriptionParser <-
    P.safeSelectionSet subscriptionRoot Nothing []
    <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))
  queryParser <- queryWithIntrospectionHelper queryFields mutationParser subscriptionParser
  pure $ GQLContext (finalizeParser queryParser) (finalizeParser <$> mutationParser)


----------------------------------------------------------------
-- Building parser fields

buildRoleBasedRemoteSchemaParser
  :: forall m
   . (MonadError QErr m, MonadUnique m, MonadIO m)
  => RoleName
  -> RemoteSchemaCache
  -> m [(RemoteSchemaName, (IntrospectionResult, ParsedIntrospection))]
buildRoleBasedRemoteSchemaParser roleName remoteSchemaCache = do
  let remoteSchemaIntroInfos = map fst $ toList remoteSchemaCache
  remoteSchemaPerms <-
    for remoteSchemaIntroInfos $ \(RemoteSchemaCtx remoteSchemaName _ remoteSchemaInfo _ _ permissions) ->
      for (Map.lookup roleName permissions) $ \introspectRes -> do
        (queryParsers, mutationParsers, subscriptionParsers) <-
             P.runSchemaT @m @(P.ParseT Identity) $ buildRemoteParser introspectRes remoteSchemaInfo
        let parsedIntrospection = ParsedIntrospection queryParsers mutationParsers subscriptionParsers
        return (remoteSchemaName, (introspectRes, parsedIntrospection))
  return $ catMaybes remoteSchemaPerms

-- checks that there are no conflicting root field names between remotes and
-- hasura fields
remoteSchemaFields
  :: forall arr m
   . ( ArrowChoice arr
     , ArrowWriter (Seq InconsistentMetadata) arr
     , ArrowKleisli m arr
     , MonadError QErr m
     )
  => ([G.Name], [G.Name], HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject))
     `arr`
     [( RemoteSchemaName , (IntrospectionResult, ParsedIntrospection))]
remoteSchemaFields = proc (queryFieldNames, mutationFieldNames, allRemoteSchemas) -> do
  (| foldlA' (\okSchemas (newSchemaName, (newSchemaContext, newMetadataObject)) -> do
       checkedDuplicates <- (| withRecordInconsistency (do
         let (queryOld, mutationOld) =
               unzip $ fmap ((\case ParsedIntrospection q m _ -> (q,m)) . snd . snd) okSchemas
         let ParsedIntrospection queryNew mutationNew _subscriptionNew
               = _rscParsed newSchemaContext
         -- Check for conflicts between remotes
         bindErrorA -<
           for_ (duplicates (fmap (P.getName . fDefinition) (queryNew ++ concat queryOld))) $
           \name -> throw400 Unexpected $ "Duplicate remote field " <> squote name
         -- Check for conflicts between this remote and the tables
         bindErrorA -<
           for_ (duplicates (fmap (P.getName . fDefinition) queryNew ++ queryFieldNames)) $
           \name -> throw400 RemoteSchemaConflicts $ "Field cannot be overwritten by remote field " <> squote name
         -- Ditto, but for mutations
         case mutationNew of
           Nothing -> returnA -< ()
           Just ms -> do
             bindErrorA -<
               for_ (duplicates (fmap (P.getName . fDefinition) (ms ++ concat (catMaybes mutationOld)))) $
               \name -> throw400 Unexpected $ "Duplicate remote field " <> squote name
             -- Ditto, but for mutations
             bindErrorA -<
               for_ (duplicates (fmap (P.getName . fDefinition) ms ++ mutationFieldNames)) $
               \name -> throw400 Unexpected $ "Field cannot be overwritten by remote field " <> squote name
         -- No need to check subscriptions as these are not supported
         returnA -< ()
         ) |) newMetadataObject
       case checkedDuplicates of
         Nothing -> returnA -< okSchemas
         Just _  -> returnA -< (newSchemaName, ( _rscIntro newSchemaContext,_rscParsed newSchemaContext)):okSchemas
     ) |) [] (Map.toList allRemoteSchemas)

buildQueryFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableCache b
  -> FunctionCache b
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildQueryFields sourceName sourceConfig tables (takeExposedAs FEAQuery -> functions) = do
  roleName <- askRoleName
  functionPermsCtx <- asks $ qcFunctionPermsContext . getter
  tableSelectExpParsers <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableGQLName <- getTableGQLName @b tableName
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    selectPerms  <- tableSelectPermissions tableName
    for selectPerms $ buildTableQueryFields sourceName sourceConfig tableName tableInfo tableGQLName
  functionSelectExpParsers <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    guard
      $ roleName == adminRoleName
      || roleName `elem` _fiPermissions functionInfo
      || functionPermsCtx == FunctionPermissionsInferred
    let targetTable = _fiReturnType functionInfo
    selectPerms <- MaybeT $ tableSelectPermissions targetTable
    lift $ buildFunctionQueryFields sourceName sourceConfig functionName functionInfo targetTable selectPerms
  pure $ concat $ catMaybes $ tableSelectExpParsers <> functionSelectExpParsers

buildRelayQueryFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableCache b
  -> FunctionCache b
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildRelayQueryFields sourceName sourceConfig tables (takeExposedAs FEAQuery -> functions) = do
  tableConnectionFields <- for (Map.toList tables) \(tableName, tableInfo) -> runMaybeT do
    tableGQLName <- getTableGQLName @b tableName
    pkeyColumns  <- hoistMaybe $ tableInfo ^? tiCoreInfo.tciPrimaryKey._Just.pkColumns
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    selectPerms  <- MaybeT $ tableSelectPermissions tableName
    MaybeT $ buildTableRelayQueryFields sourceName sourceConfig tableName tableInfo tableGQLName pkeyColumns selectPerms
  functionConnectionFields <- for (Map.toList functions) $ \(functionName, functionInfo) -> runMaybeT do
    let returnTable = _fiReturnType functionInfo
    -- FIXME: only extract the TableInfo once to avoid redundant cache lookups
    pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns) <$> askTableInfo returnTable
    selectPerms <- MaybeT $ tableSelectPermissions returnTable
    MaybeT $ buildFunctionRelayQueryFields sourceName sourceConfig functionName functionInfo returnTable pkeyColumns selectPerms
  pure $ catMaybes $ tableConnectionFields <> functionConnectionFields

buildMutationFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => Scenario
  -> SourceName
  -> SourceConfig b
  -> TableCache b
  -> FunctionCache b
  -> m [P.FieldParser n (MutationRootField UnpreparedValue)]
buildMutationFields scenario sourceName sourceConfig tables (takeExposedAs FEAMutation -> functions) = do
  roleName <- askRoleName
  tableMutations <- for (Map.toList tables) \(tableName, tableInfo) -> do
    tableGQLName  <- getTableGQLName @b tableName
    -- FIXME: retrieve permissions directly from tableInfo to avoid a sourceCache lookup
    tablePerms    <- tablePermissions tableName
    for tablePerms \RolePermInfo{..} -> do
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
          buildTableInsertMutationFields sourceName sourceConfig tableName tableInfo
                                 tableGQLName insertPerms _permSel _permUpd
      updates <- runMaybeT $ do
        guard $ isMutable viIsUpdatable viewInfo
        updatePerms <- hoistMaybe _permUpd
        lift $ buildTableUpdateMutationFields sourceName sourceConfig tableName tableInfo tableGQLName updatePerms _permSel
      deletes <- runMaybeT $ do
        guard $ isMutable viIsDeletable viewInfo
        deletePerms <- hoistMaybe _permDel
        lift $ buildTableDeleteMutationFields sourceName sourceConfig tableName tableInfo tableGQLName deletePerms _permSel
      pure $ concat $ catMaybes [inserts, updates, deletes]
  functionMutations <- for (Map.toList functions) \(functionName, functionInfo) -> runMaybeT $ do
    let targetTable = _fiReturnType functionInfo
    selectPerms <- MaybeT $ tableSelectPermissions targetTable
    -- A function exposed as mutation must have a function permission
    -- configured for the role. See Note [Function Permissions]
    guard $
      -- when function permissions are inferred, we don't expose the
      -- mutation functions for non-admin roles. See Note [Function Permissions]
      roleName == adminRoleName || roleName `elem` (_fiPermissions functionInfo)
    lift $ buildFunctionMutationFields sourceName sourceConfig functionName functionInfo targetTable selectPerms
  pure $ concat $ catMaybes $ tableMutations <> functionMutations



----------------------------------------------------------------
-- Building root parser from fields

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> [P.FieldParser n RemoteField]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
buildQueryParser pgQueryFields remoteFields allActions nonObjectCustomTypes mutationParser subscriptionParser = do
  actionQueryFields <- concat <$> traverse (buildActionQueryFields nonObjectCustomTypes) allActions
  let allQueryFields = pgQueryFields <> actionQueryFields <> map (fmap RFRemote) remoteFields
  queryWithIntrospectionHelper allQueryFields mutationParser subscriptionParser

queryWithIntrospectionHelper
  :: (MonadSchema n m, MonadError QErr m)
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP = do
  basicQueryP <- queryRootFromFields basicQueryFP
  emptyIntro  <- emptyIntrospection
  allBasicTypes <- collectTypes $
    [ P.parserType basicQueryP
    , P.parserType subscriptionP
    ]
    ++ maybeToList (P.parserType <$> mutationP)
  allIntrospectionTypes <- collectTypes . P.parserType =<< queryRootFromFields emptyIntro
  let allTypes = Map.unions
        [ allBasicTypes
        , Map.filterWithKey (\name _info -> name /= queryRoot) allIntrospectionTypes
        ]
      partialSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = P.parserType basicQueryP
        , sMutationType = P.parserType <$> mutationP
        , sSubscriptionType = Just $ P.parserType subscriptionP
        , sDirectives = defaultDirectives
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  P.safeSelectionSet queryRoot Nothing partialQueryFields
    <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))

queryRootFromFields
  :: forall n m
   . (MonadError QErr m, MonadParse n)
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryRootFromFields fps =
  P.safeSelectionSet queryRoot Nothing fps
    <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))

emptyIntrospection
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => m [P.FieldParser n (QueryRootField UnpreparedValue)]
emptyIntrospection = do
  emptyQueryP <- queryRootFromFields @n []
  introspectionTypes <- collectTypes (P.parserType emptyQueryP)
  let introspectionSchema = Schema
        { sDescription = Nothing
        , sTypes = introspectionTypes
        , sQueryType = P.parserType emptyQueryP
        , sMutationType = Nothing
        , sSubscriptionType = Nothing
        , sDirectives = mempty
        }
  return $ fmap (fmap RFRaw) [schema introspectionSchema, typeIntrospection introspectionSchema]

collectTypes
  :: forall m a
   . (MonadError QErr m, P.HasTypeDefinitions a)
  => a
  -> m (HashMap G.Name (P.Definition P.SomeTypeInfo))
collectTypes x = P.collectTypeDefinitions x
  `onLeft` \(P.ConflictingDefinitions (type1, origin1) (_type2, origins)) -> throw500 $
    "Found conflicting definitions for " <> P.getName type1 <<> ".  The definition at " <> origin1 <<>
    " differs from the the definition at " <> commaSeparated origins <<> "."

-- | Prepare the parser for subscriptions. Every postgres query field is
-- exposed as a subscription along with fields to get the status of
-- asynchronous actions.
buildSubscriptionParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> [ActionInfo]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
buildSubscriptionParser queryFields allActions = do
  actionSubscriptionFields <- concat <$> traverse buildActionSubscriptionFields allActions
  let subscriptionFields = queryFields <> actionSubscriptionFields
  P.safeSelectionSet subscriptionRoot Nothing subscriptionFields
         <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))

buildMutationParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => [P.FieldParser n RemoteField]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> [P.FieldParser n (MutationRootField UnpreparedValue)]
  -> m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))))
buildMutationParser allRemotes allActions nonObjectCustomTypes mutationFields = do
  actionParsers <- concat <$> traverse (buildActionMutationFields nonObjectCustomTypes) allActions
  let mutationFieldsParser =
        mutationFields <>
        actionParsers <>
        fmap (fmap RFRemote) allRemotes
  if null mutationFieldsParser
  then pure Nothing
  else P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFieldsParser
            <&> Just . fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))



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

runMonadSchema
  :: forall b m a
   . Monad m
  => RoleName
  -> QueryContext
  -> SourceCache
  -> BackendExtension b
  -> P.SchemaT
       (P.ParseT Identity)
       (ReaderT ( RoleName
                , SourceCache
                , QueryContext
                , BackendExtension b
                ) m
       ) a
  -> m a
runMonadSchema roleName queryContext pgSources extensions m =
  flip runReaderT (roleName, pgSources, queryContext, extensions) $ P.runSchemaT m

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

type RemoteSchemaCache = HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)

buildBackendSource
  :: (forall b. BackendSchema b => SourceInfo b -> r)
  -> AB.AnyBackend SourceInfo
  -> r
buildBackendSource f e = AB.dispatchAnyBackend @BackendSchema e f
