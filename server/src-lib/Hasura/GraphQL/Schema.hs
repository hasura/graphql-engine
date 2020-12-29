{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as Set
import qualified Language.GraphQL.Draft.Syntax          as G

import           Control.Arrow.Extended
import           Control.Lens.Extended
import           Control.Monad.Unique
import           Data.Has
import           Data.List.Extended                     (duplicates)

import qualified Hasura.Backends.Postgres.Execute.Types as PG
import qualified Hasura.Backends.Postgres.SQL.Types     as PG
import qualified Hasura.GraphQL.Parser                  as P
import qualified Hasura.GraphQL.Schema.Postgres         as PGS

import           Data.Text.Extended
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Parser                  (Kind (..), Parser, Schema (..),
                                                         UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser  (FieldParser (..))
import           Hasura.GraphQL.Schema.Action
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Introspect
import           Hasura.GraphQL.Schema.Mutation
import           Hasura.GraphQL.Schema.Remote           (buildRemoteParser)
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.Session


-- Mapping from backend to schema.
-- Those instances are orphan by design: generic parsers must be written with the knowledge of the
-- BackendSchema typeclass, and the backend-specific parsers that we specify here do in turn rely on
-- those generic parsers. To avoid a include loop, we split the definition of the typeclass and of
-- its instance.

instance BackendSchema 'Postgres where
  columnParser              = PGS.columnParser
  jsonPathArg               = PGS.jsonPathArg
  getTableGQLName           = PGS.getTableGQLName
  orderByOperators          = PGS.orderByOperators
  comparisonExps            = PGS.comparisonExps
  parseScalarValue          = parsePGScalarValue
  offsetParser              = PGS.offsetParser
  mkCountType               = PGS.mkCountType
  aggregateOrderByCountType = PG.PGInteger
  computedField             = computedFieldPG
  node                      = nodePG

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

type RemoteSchemaCache = HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)

buildGQLContext
  :: forall arr m
   . ( ArrowChoice arr
     , ArrowWriter (Seq InconsistentMetadata) arr
     , ArrowKleisli m arr
     , MonadError QErr m
     , MonadIO m
     , MonadUnique m
     , HasSQLGenCtx m
     , HasRemoteSchemaPermsCtx m
     )
  => ( GraphQLQueryType
     , SourceCache 'Postgres
     , RemoteSchemaCache
     , ActionCache
     , NonObjectTypeMap
     )
     `arr`
     ( HashMap RoleName (RoleContext GQLContext)
     , GQLContext
     )
buildGQLContext =
  proc (queryType, pgSources, allRemoteSchemas, allActions, nonObjectCustomTypes) -> do

    SQLGenCtx{ stringifyNum } <- bindA -< askSQLGenCtx
    remoteSchemaPermsCtx <- bindA -< askRemoteSchemaPermsCtx

    let remoteSchemasRoles = concatMap (Map.keys . _rscPermissions . fst . snd) $ Map.toList allRemoteSchemas

    let allRoles = Set.insert adminRoleName $
             (pgSources ^.. folded.to _pcTables.folded.tiRolePermInfoMap.to Map.keys.folded)
          <> (allActionInfos ^.. folded.aiPermissions.to Map.keys.folded)
          <> Set.fromList (bool mempty remoteSchemasRoles $ remoteSchemaPermsCtx == RemoteSchemaPermsEnabled)
        allActionInfos = Map.elems allActions
        queryRemotesMap =
          fmap (map fDefinition . piQuery . _rscParsed . fst) allRemoteSchemas
        queryContext = QueryContext stringifyNum queryType queryRemotesMap

    -- build the admin DB-only context so that we can check against name clashes with remotes
    -- TODO: Is there a better way to check for conflicts without actually building the admin schema?
    adminHasuraDBContext <- bindA -<
      buildFullestDBSchema queryContext pgSources allActionInfos nonObjectCustomTypes

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

    let adminQueryRemotes = concatMap (piQuery . snd) remotes
        adminMutationRemotes = concatMap (concat . piMutation . snd) remotes

    roleContexts <- bindA -<
      ( Set.toMap allRoles & Map.traverseWithKey \roleName () ->
          case queryType of
            QueryHasura ->
              buildRoleContext queryContext pgSources allRemoteSchemas allActionInfos
              nonObjectCustomTypes remotes roleName remoteSchemaPermsCtx
            QueryRelay ->
              buildRelayRoleContext queryContext pgSources allActionInfos
              nonObjectCustomTypes adminMutationRemotes roleName
      )
    unauthenticated <- bindA -< unauthenticatedContext adminQueryRemotes adminMutationRemotes remoteSchemaPermsCtx
    returnA -< (roleContexts, unauthenticated)

runMonadSchema
  :: (Monad m)
  => RoleName
  -> QueryContext
  -> SourceCache 'Postgres
  -> P.SchemaT (P.ParseT Identity) (ReaderT (RoleName, SourceCache 'Postgres, QueryContext) m) a -> m a
runMonadSchema roleName queryContext pgSources m =
  flip runReaderT (roleName, pgSources, queryContext) $ P.runSchemaT m

buildRoleBasedRemoteSchemaParser
  :: forall m
   . (MonadError QErr m, MonadUnique m, MonadIO m)
  => RoleName
  -> RemoteSchemaCache
  -> m [(RemoteSchemaName, ParsedIntrospection)]
buildRoleBasedRemoteSchemaParser role remoteSchemaCache = do
  let remoteSchemaIntroInfos = map fst $ toList remoteSchemaCache
  remoteSchemaPerms <-
    for remoteSchemaIntroInfos $ \(RemoteSchemaCtx remoteSchemaName _ remoteSchemaInfo _ _ permissions) ->
      for (Map.lookup role permissions) $ \introspectRes -> do
        (queryParsers, mutationParsers, subscriptionParsers) <-
             P.runSchemaT @m @(P.ParseT Identity) $ buildRemoteParser introspectRes remoteSchemaInfo
        let parsedIntrospection = ParsedIntrospection queryParsers mutationParsers subscriptionParsers
        return $ (remoteSchemaName, parsedIntrospection)
  return $ catMaybes remoteSchemaPerms

-- TODO: Integrate relay schema
buildRoleContext
  :: (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> SourceCache 'Postgres -> RemoteSchemaCache
  -> [ActionInfo 'Postgres] -> NonObjectTypeMap
  -> [( RemoteSchemaName , ParsedIntrospection)]
  -> RoleName
  -> RemoteSchemaPermsCtx
  -> m (RoleContext GQLContext)
buildRoleContext queryContext pgSources
  allRemoteSchemas allActionInfos nonObjectCustomTypes remotes roleName remoteSchemaPermsCtx = do

  roleBasedRemoteSchemas <-
    if | roleName == adminRoleName                        -> pure remotes
       | remoteSchemaPermsCtx == RemoteSchemaPermsEnabled -> buildRoleBasedRemoteSchemaParser roleName allRemoteSchemas
       -- when remote schema permissions are not enabled, then remote schemas
       -- are a public entity which is accesible to all the roles
       | otherwise                                        -> pure remotes

  let queryRemotes    = getQueryRemotes $ snd <$> roleBasedRemoteSchemas
      mutationRemotes = getMutationRemotes $ snd <$> roleBasedRemoteSchemas

  runMonadSchema roleName queryContext pgSources $ do
    fieldsList <- forM (toList pgSources) $ \(SourceInfo sourceName tables functions sourceConfig) -> do
      let validTables = takeValidTables tables
          validFunctions = takeValidFunctions functions
          tableNames = Map.keysSet validTables
          functionsWithSourceConfig = map (, (sourceName, sourceConfig)) validFunctions
      (functionsWithSourceConfig,,,)
        <$> buildPostgresQueryFields sourceName sourceConfig tableNames validFunctions
        <*> buildPGMutationFields Frontend sourceName sourceConfig tableNames
        <*> buildPGMutationFields Backend sourceName sourceConfig tableNames

    let (allFunctions, queryPGFields, mutationFrontendFields, mutationBackendFields) = mconcat fieldsList

    mutationParserFrontend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes allFunctions mutationFrontendFields

    mutationParserBackend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes allFunctions mutationBackendFields

    subscriptionParser <- buildSubscriptionParser queryPGFields allActionInfos

    queryParserFrontend <- buildQueryParser queryPGFields queryRemotes
      allActionInfos nonObjectCustomTypes mutationParserFrontend subscriptionParser
    queryParserBackend <- buildQueryParser queryPGFields queryRemotes
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

-- TODO why do we do these validations at this point? What does it mean to track
--      a function but not add it to the schema...?
--      Auke:
--        I believe the intention is simply to allow the console to do postgres data management
--      Karthikeyan: Yes, this is correct. We allowed this pre PDV but somehow
--        got removed in PDV. OTOH, I’m not sure how prevalent this feature
--        actually is
takeValidTables :: TableCache 'Postgres -> TableCache 'Postgres
takeValidTables = Map.filterWithKey graphQLTableFilter . Map.filter tableFilter
  where
    tableFilter = not . isSystemDefined . _tciSystemDefined . _tiCoreInfo
    graphQLTableFilter tableName tableInfo =
      -- either the table name should be GraphQL compliant
      -- or it should have a GraphQL custom name set with it
      PG.isGraphQLCompliantTableName tableName
      || (isJust . _tcCustomName . _tciCustomConfig . _tiCoreInfo $ tableInfo)

-- TODO and what about graphql-compliant function names here too?
takeValidFunctions :: FunctionCache -> [FunctionInfo]
takeValidFunctions = Map.elems . Map.filter functionFilter
  where
    functionFilter = not . isSystemDefined . fiSystemDefined

takeExposedAs :: FunctionExposedAs -> (a -> FunctionInfo) -> [a] -> [a]
takeExposedAs x f = filter ((== x) . fiExposedAs . f)


buildFullestDBSchema
  :: (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> SourceCache 'Postgres -> [ActionInfo 'Postgres] -> NonObjectTypeMap
  -> m ( Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres)))
       , Maybe (Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres))))
       )
buildFullestDBSchema queryContext pgSources allActionInfos nonObjectCustomTypes =
  runMonadSchema adminRoleName queryContext pgSources $ do
    fieldsList <- forM (toList pgSources) $ \(SourceInfo sourceName tables functions sourceConfig) -> do
      let validTables = takeValidTables tables
          validFunctions = takeValidFunctions functions
          tableNames = Map.keysSet validTables
          functionsWithSourceConfig = map (, (sourceName, sourceConfig)) validFunctions
      (functionsWithSourceConfig,,)
        <$> buildPGMutationFields Frontend sourceName sourceConfig tableNames
        <*> buildPostgresQueryFields sourceName sourceConfig tableNames validFunctions

    let (allFunctions, mutationPGFields, queryPGFields) = mconcat fieldsList

    mutationParserFrontend <-
      -- NOTE: we omit remotes here on purpose since we're trying to check name
      -- clashes with remotes:
      buildMutationParser mempty allActionInfos nonObjectCustomTypes allFunctions mutationPGFields

    subscriptionParser <- buildSubscriptionParser queryPGFields allActionInfos

    queryParserFrontend <- buildQueryParser queryPGFields mempty
      allActionInfos nonObjectCustomTypes mutationParserFrontend subscriptionParser

    pure (queryParserFrontend, mutationParserFrontend)

buildRelayRoleContext
  :: (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> SourceCache 'Postgres -> [ActionInfo 'Postgres] -> NonObjectTypeMap
  -> [P.FieldParser (P.ParseT Identity) RemoteField]
  -> RoleName
  -> m (RoleContext GQLContext)
buildRelayRoleContext queryContext pgSources
  allActionInfos nonObjectCustomTypes mutationRemotes roleName =

  runMonadSchema roleName queryContext pgSources $ do
    fieldsList <- forM (toList pgSources) $ \(SourceInfo sourceName tables functions sourceConfig) -> do
      let validTables = takeValidTables tables
          validFunctions = takeValidFunctions functions
          tableNames = Map.keysSet validTables
          functionsWithSourceConfig = map (, (sourceName, sourceConfig)) validFunctions
      (functionsWithSourceConfig,,,)
        <$> buildRelayPostgresQueryFields sourceName sourceConfig tableNames validFunctions
        <*> buildPGMutationFields Frontend sourceName sourceConfig tableNames
        <*> buildPGMutationFields Backend sourceName sourceConfig tableNames

    -- Add node root field
    nodeField_ <- nodeField
    let (allFunctions, queryPGFields', mutationFrontendFields, mutationBackendFields) = mconcat fieldsList
        queryPGFields = nodeField_:queryPGFields'

    mutationParserFrontend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes allFunctions mutationFrontendFields

    mutationParserBackend <-
      buildMutationParser mutationRemotes allActionInfos nonObjectCustomTypes allFunctions mutationBackendFields

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

finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

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
     [( RemoteSchemaName , ParsedIntrospection)]
remoteSchemaFields = proc (queryFieldNames, mutationFieldNames, allRemoteSchemas) -> do
  (| foldlA' (\okSchemas (newSchemaName, (newSchemaContext, newMetadataObject)) -> do
       checkedDuplicates <- (| withRecordInconsistency (do
         let (queryOld, mutationOld) =
               unzip $ fmap ((\case ParsedIntrospection q m _ -> (q,m)) . snd) okSchemas
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
         Just _  -> returnA -< (newSchemaName, _rscParsed newSchemaContext):okSchemas
     ) |) [] (Map.toList allRemoteSchemas)

buildPostgresQueryFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => SourceName
  -> SourceConfig 'Postgres
  -> HashSet PG.QualifiedTable
  -> [FunctionInfo]
  -> m [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
buildPostgresQueryFields sourceName sourceConfig allTables (takeExposedAs FEAQuery id -> queryFunctions) = do
  tableSelectExpParsers <- for (toList allTables) \table -> do
    selectPerms <- tableSelectPermissions table
    customRootFields <- _tcCustomRootFields . _tciCustomConfig . _tiCoreInfo <$> askTableInfo @'Postgres table
    for selectPerms \perms -> do
      tableGQLName <- getTableGQLName @'Postgres table
      let fieldsDesc = G.Description $ "fetch data from the table: " <>> table
          aggName = tableGQLName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "fetch aggregated fields from the table: " <>> table
          pkName = tableGQLName <> $$(G.litName "_by_pk")
          pkDesc = G.Description $ "fetch data from the table: " <> table <<> " using primary key columns"
      catMaybes <$> sequenceA
        [ requiredFieldParser (asDbRootField . QDBSimple)      $ selectTable          table (fromMaybe tableGQLName $ _tcrfSelect          customRootFields) (Just fieldsDesc) perms
        , mapMaybeFieldParser (asDbRootField . QDBPrimaryKey)  $ selectTableByPk      table (fromMaybe pkName       $ _tcrfSelectByPk      customRootFields) (Just pkDesc)     perms
        , mapMaybeFieldParser (asDbRootField . QDBAggregation) $ selectTableAggregate table (fromMaybe aggName      $ _tcrfSelectAggregate customRootFields) (Just aggDesc)    perms
        ]
  functionSelectExpParsers <- for queryFunctions \function -> do
    let targetTable = fiReturnType function
        functionName = fiName function
    selectPerms <- tableSelectPermissions targetTable
    for selectPerms \perms -> do
      displayName <- PG.qualifiedObjectToName functionName
      let functionDesc = G.Description $ "execute function " <> functionName <<> " which returns " <>> targetTable
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> targetTable
      catMaybes <$> sequenceA
        [ requiredFieldParser (asDbRootField . QDBSimple)      $ selectFunction          function displayName (Just functionDesc) perms
        , mapMaybeFieldParser (asDbRootField . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms
        ]
  pure $ (concat . catMaybes) (tableSelectExpParsers <> functionSelectExpParsers)
  where
    asDbRootField =
      let pgExecCtx = PG._pscExecCtx sourceConfig
      in RFDB sourceName pgExecCtx

    mapMaybeFieldParser :: (a -> b) -> m (Maybe (P.FieldParser n a)) -> m (Maybe (P.FieldParser n b))
    mapMaybeFieldParser f = fmap $ fmap $ fmap f

requiredFieldParser
  :: (Functor n, Functor m)=> (a -> b) -> m (P.FieldParser n a) -> m (Maybe (P.FieldParser n b))
requiredFieldParser f = fmap $ Just . fmap f


-- | Includes remote schema fields and actions
buildActionQueryFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [ActionInfo 'Postgres]
  -> NonObjectTypeMap
  -> m [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
buildActionQueryFields allActions nonObjectCustomTypes = do
  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionSynchronous -> pure Nothing
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo
      ActionQuery ->
        fmap (fmap (RFAction . AQQuery)) <$> actionExecute nonObjectCustomTypes actionInfo
  pure $ catMaybes actionParsers

buildActionSubscriptionFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [ActionInfo 'Postgres]
  -> m [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
buildActionSubscriptionFields allActions = do
  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo
      ActionMutation ActionSynchronous -> pure Nothing
      ActionQuery -> pure Nothing
  pure $ catMaybes actionParsers

buildRelayPostgresQueryFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => SourceName
  -> SourceConfig 'Postgres
  -> HashSet PG.QualifiedTable
  -> [FunctionInfo]
  -> m [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
buildRelayPostgresQueryFields sourceName sourceConfig allTables (takeExposedAs FEAQuery id -> queryFunctions) = do
  tableConnectionFields <- for (toList allTables) $ \table -> runMaybeT do
    pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                   <$> askTableInfo table
    selectPerms <- MaybeT $ tableSelectPermissions table
    tableGQLName <- getTableGQLName @'Postgres table
    let fieldName = tableGQLName <> $$(G.litName "_connection")
        fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> table
    lift $ selectTableConnection table fieldName fieldDesc pkeyColumns selectPerms

  functionConnectionFields <- for queryFunctions $ \function -> runMaybeT do
    let returnTable = fiReturnType function
        functionName = fiName function
    pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                   <$> askTableInfo returnTable
    selectPerms <- MaybeT $ tableSelectPermissions returnTable
    displayName <- PG.qualifiedObjectToName functionName
    let fieldName = displayName <> $$(G.litName "_connection")
        fieldDesc = Just $ G.Description $ "execute function " <> functionName
                    <<> " which returns " <>> returnTable
    lift $ selectFunctionConnection function fieldName fieldDesc pkeyColumns selectPerms

  pure $ map (fmap (asDbRootField . QDBConnection)) $ catMaybes $
         tableConnectionFields <> functionConnectionFields
  where
    asDbRootField =
      let pgExecCtx = PG._pscExecCtx sourceConfig
      in RFDB sourceName pgExecCtx

queryRootFromFields
  :: forall n m
   . (MonadError QErr m, MonadParse n)
  => [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres))))
queryRootFromFields fps =
  P.safeSelectionSet queryRoot Nothing fps
    <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))

emptyIntrospection
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => m [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
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
collectTypes x = case P.collectTypeDefinitions x of
  Left (P.ConflictingDefinitions (type1, origin1) (_type2, origins)) -> throw500 $
    "Found conflicting definitions for " <> P.getName type1
    <<> ".  The definition at " <> origin1 <<> " differs from the the definition at " <>> commaSeparated origins
  Right tps -> pure tps

queryWithIntrospectionHelper
  :: (MonadSchema n m, MonadError QErr m)
  => [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres))))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres)))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres))))
queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP = do
  basicQueryP <- queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
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

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
  -> [P.FieldParser n RemoteField]
  -> [ActionInfo 'Postgres]
  -> NonObjectTypeMap
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres))))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres)))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres))))
buildQueryParser pgQueryFields remoteFields allActions nonObjectCustomTypes mutationParser subscriptionParser = do
  actionQueryFields <- buildActionQueryFields allActions nonObjectCustomTypes
  let allQueryFields = pgQueryFields <> actionQueryFields <> map (fmap RFRemote) remoteFields
  queryWithIntrospectionHelper allQueryFields mutationParser subscriptionParser

-- | Prepare the parser for subscriptions. Every postgres query field is
-- exposed as a subscription along with fields to get the status of
-- asynchronous actions.
buildSubscriptionParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo 'Postgres r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [P.FieldParser n (QueryRootField (UnpreparedValue 'Postgres))]
  -> [ActionInfo 'Postgres]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres))))
buildSubscriptionParser pgQueryFields allActions = do
  actionSubscriptionFields <- buildActionSubscriptionFields allActions
  let subscriptionFields = pgQueryFields <> actionSubscriptionFields
  P.safeSelectionSet subscriptionRoot Nothing subscriptionFields
         <&> fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))

buildPGMutationFields
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => Scenario -> SourceName -> SourceConfig 'Postgres -> HashSet PG.QualifiedTable
  -> m [P.FieldParser n (MutationRootField (UnpreparedValue 'Postgres))]
buildPGMutationFields scenario sourceName sourceConfig allTables = do
  concat . catMaybes <$> for (toList allTables) \table -> do
    tableCoreInfo <- _tiCoreInfo <$> askTableInfo @'Postgres table
    tableGQLName   <- getTableGQLName @'Postgres table
    tablePerms    <- tablePermissions table
    for tablePerms \RolePermInfo{..} -> do
      let customRootFields = _tcCustomRootFields $ _tciCustomConfig tableCoreInfo
          viewInfo         = _tciViewInfo tableCoreInfo

      -- If we're in a frontend scenario, we should not include backend_only inserts
      let scenarioInsertPermissionM = do
            insertPermission <- _permIns
            if scenario == Frontend && ipiBackendOnly insertPermission
              then Nothing
              else return insertPermission

      inserts <- fmap join $ whenMaybe (isMutable viIsInsertable viewInfo) $ for scenarioInsertPermissionM \insertPerms -> do
        let insertName = $$(G.litName "insert_") <> tableGQLName
            insertDesc = G.Description $ "insert data into the table: " <>> table
            insertOneName = $$(G.litName "insert_") <> tableGQLName <> $$(G.litName "_one")
            insertOneDesc = G.Description $ "insert a single row into the table: " <>> table
        insert <- insertIntoTable table (fromMaybe insertName $ _tcrfInsert customRootFields) (Just insertDesc) insertPerms _permSel _permUpd
        -- select permissions are required for InsertOne: the
        -- selection set is the same as a select on that table, and it
        -- therefore can't be populated if the user doesn't have
        -- select permissions
        insertOne <- for _permSel \selPerms ->
          insertOneIntoTable table (fromMaybe insertOneName $ _tcrfInsertOne customRootFields) (Just insertOneDesc) insertPerms selPerms _permUpd
        pure $ fmap (asDbRootField . MDBInsert) <$> insert : maybeToList insertOne

      updates <- fmap join $ whenMaybe (isMutable viIsUpdatable viewInfo) $ for _permUpd \updatePerms -> do
        let updateName = $$(G.litName "update_") <> tableGQLName
            updateDesc = G.Description $ "update data of the table: " <>> table
            updateByPkName = $$(G.litName "update_") <> tableGQLName <> $$(G.litName "_by_pk")
            updateByPkDesc = G.Description $ "update single row of the table: " <>> table
        update <- updateTable table (fromMaybe updateName $ _tcrfUpdate customRootFields) (Just updateDesc) updatePerms _permSel
        -- likewise; furthermore, primary keys can only be tested in
        -- the `where` clause if the user has select permissions for
        -- them, which at the very least requires select permissions
        updateByPk <- join <$> for _permSel
          (updateTableByPk table (fromMaybe updateByPkName $ _tcrfUpdateByPk customRootFields) (Just updateByPkDesc) updatePerms)
        pure $ fmap (asDbRootField . MDBUpdate) <$> catMaybes [update, updateByPk]

      -- when the table/view is mutable and there exists a delete permission
      deletes <- fmap join $ whenMaybe (isMutable viIsDeletable viewInfo) $
        for _permDel $ \deletePermission -> do
          delete <- buildDeleteField table tableGQLName (_tcrfDelete customRootFields)
            deletePermission _permSel
          -- select permission is needed for deleteByPk field so that a return type
          -- for the field can be generated
          deleteByPk <- fmap join $ for _permSel $
            buildDeleteByPkField table tableGQLName (_tcrfDeleteByPk customRootFields) deletePermission

          pure $ fmap (asDbRootField . MDBDelete) <$> delete : maybeToList deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]

  where
    asDbRootField =
      let pgExecCtx = PG._pscExecCtx sourceConfig
      in RFDB sourceName pgExecCtx

    buildDeleteField table tableGQLName customName deletePermission selectPermission = do
      let deleteName = $$(G.litName "delete_") <> tableGQLName
          deleteDesc = G.Description $ "delete data from the table: " <>> table
      deleteFromTable table (fromMaybe deleteName customName) (Just deleteDesc)
        deletePermission selectPermission

    buildDeleteByPkField table tableGQLName customName deletePermission = do
      let fieldName = $$(G.litName "delete_") <> tableGQLName <> $$(G.litName "_by_pk")
          fieldDescription = G.Description $ "delete single row from the table: " <>> table
      deleteFromTableByPk table (fromMaybe fieldName customName) (Just fieldDescription) deletePermission

subscriptionRoot :: G.Name
subscriptionRoot = $$(G.litName "subscription_root")

mutationRoot :: G.Name
mutationRoot = $$(G.litName "mutation_root")

queryRoot :: G.Name
queryRoot = $$(G.litName "query_root")

buildMutationParser
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => [P.FieldParser n RemoteField]
  -> [ActionInfo 'Postgres]
  -> NonObjectTypeMap
  -> [(FunctionInfo, (SourceName, SourceConfig 'Postgres))]
  -- ^ all "valid" functions
  -> [P.FieldParser n (MutationRootField (UnpreparedValue 'Postgres))]
  -> m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres)))))
buildMutationParser allRemotes allActions nonObjectCustomTypes
    (takeExposedAs FEAMutation fst -> mutationFunctions) pgMutationFields = do

   -- NOTE: this is basically copied from functionSelectExpParsers body
  functionMutationExpParsers <- for mutationFunctions \(function@FunctionInfo{..}, (sourceName, sourceConfig)) -> do
    selectPerms <- tableSelectPermissions fiReturnType
    for selectPerms \perms -> do
      displayName <- PG.qualifiedObjectToName fiName
      let functionDesc = G.Description $
            "execute VOLATILE function " <> fiName <<> " which returns " <>> fiReturnType
          asDbRootField =
            let pgExecCtx = PG._pscExecCtx sourceConfig
            in RFDB sourceName pgExecCtx

      catMaybes <$> sequenceA
        [ requiredFieldParser (asDbRootField . MDBFunction) $
            selectFunction function displayName (Just functionDesc) perms
        -- FWIW: The equivalent of this is possible for mutations; do we want that?:
        -- , mapMaybeFieldParser (asDbRootField . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms
        ]

  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionSynchronous ->
        fmap (fmap (RFAction . AMSync)) <$> actionExecute nonObjectCustomTypes actionInfo
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation nonObjectCustomTypes actionInfo
      ActionQuery -> pure Nothing

  let mutationFieldsParser =
        pgMutationFields <>
        concat (catMaybes functionMutationExpParsers) <>
        catMaybes actionParsers <>
        fmap (fmap RFRemote) allRemotes
  if null mutationFieldsParser
  then pure Nothing
  else P.safeSelectionSet mutationRoot (Just $ G.Description "mutation root") mutationFieldsParser
            <&> Just . fmap (fmap (P.handleTypename (RFRaw . J.String . G.unName)))
