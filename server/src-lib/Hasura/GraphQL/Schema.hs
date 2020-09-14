{-# LANGUAGE Arrows #-}
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

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Parser                 (Kind (..), Parser, Schema (..),
                                                        UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import           Hasura.GraphQL.Schema.Action
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Introspect
import           Hasura.GraphQL.Schema.Mutation
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

buildGQLContext
  :: forall arr m
   . ( ArrowChoice arr
     , ArrowWriter (Seq InconsistentMetadata) arr
     , ArrowKleisli m arr
     , MonadError QErr m
     , MonadIO m
     , MonadUnique m
     , HasSQLGenCtx m
     )
  => ( GraphQLQueryType
     , PGSourcesCache
     , HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)
     , ActionCache
     , NonObjectTypeMap
     )
     `arr`
     ( HashMap RoleName (RoleContext GQLContext)
     , GQLContext
     )
buildGQLContext =
  proc (queryType, pgSourcesCache, allRemoteSchemas, allActions, nonObjectCustomTypes) -> do

    SQLGenCtx{ stringifyNum } <- bindA -< askSQLGenCtx

    let allRoles = Set.insert adminRoleName $
             (pgSourcesCache ^.. folded.to _pcTables.folded.tiRolePermInfoMap.to Map.keys.folded)
          <> (allActionInfos ^.. folded.aiPermissions.to Map.keys.folded)
        allActionInfos = Map.elems allActions
        queryRemotesMap =
          fmap (map fDefinition . piQuery . rscParsed . fst) allRemoteSchemas
        queryContext = QueryContext stringifyNum queryType queryRemotesMap

    -- TODO: check for field name conflicts at the root level
    let queryFieldNames = []
        mutationFieldNames = []

    -- adminHasuraContext <- bindA -<
    --   runMonadSchema adminRoleName validTables Frontend $
    --     (,)
    --     <$> queryWithIntrospection validTableNames validFunctions mempty mempty
    --         allActionInfos nonObjectCustomTypes
    --     <*> mutation validTableNames mempty allActionInfos nonObjectCustomTypes Frontend

    -- queryFieldNames :: [G.Name] <- bindA -<
    --   case P.discardNullability $ P.parserType $ fst adminHasuraContext of
    --     -- It really ought to be this case; anything else is a programming error.
    --     P.TNamed (P.Definition _ _ _ (P.TIObject (P.ObjectInfo rootFields _interfaces))) ->
    --       pure $ fmap P.dName rootFields
    --     _ -> throw500 "We encountered an root query of unexpected GraphQL type.  It should be an object type."

    -- let mutationFieldNames :: [G.Name]
    --     mutationFieldNames =
    --       case P.discardNullability . P.parserType <$> snd adminHasuraContext of
    --         Just (P.TNamed def) ->
    --           case P.dInfo def of
    --             -- It really ought to be this case; anything else is a programming error.
    --             P.TIObject (P.ObjectInfo rootFields _interfaces) -> fmap P.dName rootFields
    --             _                                                -> []
    --         _ -> []

    -- This block of code checks that there are no conflicting root field names between remotes.
    remotes <- remoteSchemaFields -< (queryFieldNames, mutationFieldNames, allRemoteSchemas)

    let queryRemotes = concatMap (piQuery . snd) remotes
        mutationRemotes = concatMap (concat . piMutation . snd) remotes

    roleContexts <- bindA -<
      ( Set.toMap allRoles & Map.traverseWithKey \roleName () ->
          case queryType of
            QueryHasura ->
              buildRoleContext queryContext pgSourcesCache allActionInfos
              nonObjectCustomTypes queryRemotes mutationRemotes roleName
            QueryRelay ->
              buildRelayRoleContext queryContext pgSourcesCache allActionInfos
              nonObjectCustomTypes queryRemotes mutationRemotes roleName
      )
    unauthenticated <- bindA -< unauthenticatedContext queryRemotes mutationRemotes
    returnA -< (roleContexts, unauthenticated)

runMonadSchema
  :: (Monad m)
  => RoleName
  -> QueryContext
  -> SourceTables
  -> P.SchemaT (P.ParseT Identity) (ReaderT (RoleName, SourceTables, QueryContext) m) a -> m a
runMonadSchema roleName queryContext sourceTables m =
  flip runReaderT (roleName, sourceTables, queryContext) $ P.runSchemaT m

buildRoleContext
  :: (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> PGSourcesCache -> [ActionInfo] -> NonObjectTypeMap
  -> [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> RoleName
  -> m (RoleContext GQLContext)
buildRoleContext queryContext pgSources allActionInfos
  nonObjectCustomTypes queryRemotes mutationRemotes roleName = do
  let sourceTables = Map.map _pcTables pgSources

  fieldsList <- forM pgSources $ \(PGSourceSchemaCache tableCache functionCache sourceConfig) ->
    runMonadSchema roleName queryContext sourceTables $
      buildPGFields sourceConfig tableCache functionCache

  let (queryPGFields, mutationFrontendFields, mutationBackendFields) =
        foldr (<>) mempty fieldsList

  runMonadSchema roleName queryContext sourceTables $ do

    mutationParserFrontend <-
      buildMutationParser mutationRemotes allActionInfos
      nonObjectCustomTypes mutationFrontendFields
    mutationParserBackend <-
      buildMutationParser mutationRemotes allActionInfos
      nonObjectCustomTypes mutationBackendFields

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

buildPGFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => PGSourceConfig
  -> TableCache
  -> FunctionCache
  -> m ( [P.FieldParser n (QueryRootField UnpreparedValue)]
       , [P.FieldParser n (MutationRootField UnpreparedValue)]
       , [P.FieldParser n (MutationRootField UnpreparedValue)]
       )
  -- ^ (query fields, frontend mutation fields, backend mutation fields)
buildPGFields sourceConfig tableCache functionCache = do
  (,,)
  <$> buildPostgresQueryFields sourceConfig validTableNames validFunctions
  <*> buildPGMutationFields Frontend sourceConfig validTableNames
  <*> buildPGMutationFields Backend sourceConfig validTableNames
  where
    tableFilter    = not . isSystemDefined . _tciSystemDefined
    functionFilter = not . isSystemDefined . fiSystemDefined
    validTables = Map.filter (tableFilter . _tiCoreInfo) tableCache
    validTableNames = Map.keysSet validTables
    validFunctions = Map.elems $ Map.filter functionFilter functionCache

buildRelayRoleContext
  :: (MonadError QErr m, MonadIO m, MonadUnique m)
  => QueryContext -> PGSourcesCache -> [ActionInfo] -> NonObjectTypeMap
  -> [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> RoleName
  -> m (RoleContext GQLContext)
buildRelayRoleContext queryContext pgSources allActionInfos
  nonObjectCustomTypes queryRemotes mutationRemotes roleName = do
  let sourceTables = Map.map _pcTables pgSources

  fieldsList <- forM pgSources $
    \(PGSourceSchemaCache tableCache functionCache sourceConfig) ->
      runMonadSchema roleName queryContext sourceTables $
      buildRelayPGFields sourceConfig tableCache functionCache

  let (queryPGFields, mutationFrontendFields, mutationBackendFields) =
        foldr (<>) mempty fieldsList

  runMonadSchema roleName queryContext sourceTables $ do
    mutationParserFrontend <-
      buildMutationParser mutationRemotes allActionInfos
      nonObjectCustomTypes mutationFrontendFields

    mutationParserBackend <-
      buildMutationParser mutationRemotes allActionInfos
      nonObjectCustomTypes mutationBackendFields

    let subscriptionParser = P.selectionSet subscriptionRoot Nothing queryPGFields
                             <&> fmap (P.handleTypename (RFRaw . J.String. G.unName))
    queryParserFrontend <- queryWithIntrospectionHelper queryPGFields
      mutationParserFrontend subscriptionParser
    queryParserBackend <- queryWithIntrospectionHelper queryPGFields
      mutationParserBackend subscriptionParser

    let frontendContext = GQLContext (finalizeParser queryParserFrontend)
                          (finalizeParser <$> mutationParserFrontend)
    let backendContext = GQLContext (finalizeParser queryParserBackend)
                         (finalizeParser <$> mutationParserBackend)
    pure $ RoleContext frontendContext $ Just backendContext


buildRelayPGFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => PGSourceConfig
  -> TableCache
  -> FunctionCache
  -> m ( [P.FieldParser n (QueryRootField UnpreparedValue)]
       , [P.FieldParser n (MutationRootField UnpreparedValue)]
       , [P.FieldParser n (MutationRootField UnpreparedValue)]
       )
  -- ^ (query fields, frontend mutation fields, backend mutation fields)
buildRelayPGFields sourceConfig tableCache functionCache = do
  (,,)
  <$> buildRelayPostgresQueryFields sourceConfig validTableNames validFunctions
  <*> buildPGMutationFields Frontend sourceConfig validTableNames
  <*> buildPGMutationFields Backend sourceConfig validTableNames
  where
    tableFilter    = not . isSystemDefined . _tciSystemDefined
    functionFilter = not . isSystemDefined . fiSystemDefined
    validTables = Map.filter (tableFilter . _tiCoreInfo) tableCache
    validTableNames = Map.keysSet validTables
    validFunctions = Map.elems $ Map.filter functionFilter functionCache

unauthenticatedContext
  :: forall m
   . ( MonadError QErr m
     , MonadIO m
     , MonadUnique m
     )
  => [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> m GQLContext
unauthenticatedContext queryRemotes mutationRemotes = P.runSchemaT $ do
  let queryFields = fmap (fmap RFRemote) queryRemotes
  queryParser <- queryWithIntrospectionHelper queryFields mutationParser subscriptionParser
  pure $ GQLContext (finalizeParser queryParser) (finalizeParser <$> mutationParser)
  where
    mutationParser =
      if null mutationRemotes
      then Nothing
      else Just $ P.selectionSet mutationRoot Nothing (fmap (fmap RFRemote) mutationRemotes)
           <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))
    subscriptionParser =
      P.selectionSet subscriptionRoot Nothing []
      <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

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
               = rscParsed newSchemaContext
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
         Just _  -> returnA -< (newSchemaName, rscParsed newSchemaContext):okSchemas
     ) |) [] (Map.toList allRemoteSchemas)

buildPostgresQueryFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => PGSourceConfig
  -> HashSet QualifiedTable
  -> [FunctionInfo]
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildPostgresQueryFields sourceConfig allTables allFunctions = do
  tableSelectExpParsers <- for (toList allTables) \table -> do
    selectPerms <- tableSelectPermissions table
    customRootFields <- _tcCustomRootFields . _tciCustomConfig . _tiCoreInfo <$> askTableInfo table
    for selectPerms \perms -> do
      displayName <- qualifiedObjectToName table
      let fieldsDesc = G.Description $ "fetch data from the table: " <>> table
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "fetch aggregated fields from the table: " <>> table
          pkName = displayName <> $$(G.litName "_by_pk")
          pkDesc = G.Description $ "fetch data from the table: " <> table <<> " using primary key columns"
      catMaybes <$> sequenceA
        [ requiredFieldParser (asDbRootField . QDBSimple)      $ selectTable          table (fromMaybe displayName $ _tcrfSelect          customRootFields) (Just fieldsDesc) perms
        , mapMaybeFieldParser (asDbRootField . QDBPrimaryKey)  $ selectTableByPk      table (fromMaybe pkName      $ _tcrfSelectByPk      customRootFields) (Just pkDesc)     perms
        , mapMaybeFieldParser (asDbRootField . QDBAggregation) $ selectTableAggregate table (fromMaybe aggName     $ _tcrfSelectAggregate customRootFields) (Just aggDesc)    perms
        ]
  functionSelectExpParsers <- for allFunctions \function -> do
    let targetTable = fiReturnType function
        functionName = fiName function
    selectPerms <- tableSelectPermissions targetTable
    for selectPerms \perms -> do
      displayName <- qualifiedObjectToName functionName
      let functionDesc = G.Description $ "execute function " <> functionName <<> " which returns " <>> targetTable
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> targetTable
      catMaybes <$> sequenceA
        [ requiredFieldParser (asDbRootField . QDBSimple)      $ selectFunction          function displayName (Just functionDesc) perms
        , mapMaybeFieldParser (asDbRootField . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms
        ]
  pure $ (concat . catMaybes) (tableSelectExpParsers <> functionSelectExpParsers)
  where
    pgExecCtx = _pscExecCtx sourceConfig
    asDbRootField = RFDB pgExecCtx

    requiredFieldParser :: (a -> b) -> m (P.FieldParser n a) -> m (Maybe (P.FieldParser n b))
    requiredFieldParser f = fmap $ Just . fmap f

    mapMaybeFieldParser :: (a -> b) -> m (Maybe (P.FieldParser n a)) -> m (Maybe (P.FieldParser n b))
    mapMaybeFieldParser f = fmap $ fmap $ fmap f

-- | Includes remote schema fields and actions
buildActionQueryFields
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [ActionInfo]
  -> NonObjectTypeMap
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
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
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [ActionInfo]
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
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
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => PGSourceConfig
  -> HashSet QualifiedTable
  -> [FunctionInfo]
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
buildRelayPostgresQueryFields sourceConfig allTables allFunctions = do
  tableConnectionFields <- for (toList allTables) $ \table -> runMaybeT do
    pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                   <$> askTableInfo table
    selectPerms <- MaybeT $ tableSelectPermissions table
    displayName <- qualifiedObjectToName table
    let fieldName = displayName <> $$(G.litName "_connection")
        fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> table
    lift $ selectTableConnection table fieldName fieldDesc pkeyColumns selectPerms

  functionConnectionFields <- for allFunctions $ \function -> runMaybeT do
    let returnTable = fiReturnType function
        functionName = fiName function
    pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                   <$> askTableInfo returnTable
    selectPerms <- MaybeT $ tableSelectPermissions returnTable
    displayName <- qualifiedObjectToName functionName
    let fieldName = displayName <> $$(G.litName "_connection")
        fieldDesc = Just $ G.Description $ "execute function " <> functionName
                    <<> " which returns " <>> returnTable
    lift $ selectFunctionConnection function fieldName fieldDesc pkeyColumns selectPerms

  nodeField_ <- fmap (asDbRootField . QDBPrimaryKey) <$> nodeField
  pure $ (:) nodeField_ $ map (fmap (asDbRootField . QDBConnection)) $ catMaybes $
         tableConnectionFields <> functionConnectionFields
  where
    asDbRootField =
      let pgExecCtx = _pscExecCtx sourceConfig
      in RFDB pgExecCtx

queryRootFromFields
  :: forall n
   . MonadParse n
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
queryRootFromFields fps =
      P.selectionSet $$(G.litName "query_root") Nothing fps
  <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

emptyIntrospection
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => m [P.FieldParser n (QueryRootField UnpreparedValue)]
emptyIntrospection = do
  let emptyQueryP = queryRootFromFields @n []
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
  Left (P.ConflictingDefinitions type1 _) -> throw500 $
    "found conflicting definitions for " <> P.getName type1
    <<> " when collecting types from the schema"
  Right tps -> pure tps

queryWithIntrospectionHelper
  :: (MonadSchema n m, MonadError QErr m)
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP = do
  let basicQueryP = queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
  allBasicTypes <- collectTypes $
    [ P.parserType basicQueryP
    , P.parserType subscriptionP
    ]
    ++ maybeToList (P.parserType <$> mutationP)
  allIntrospectionTypes <- collectTypes (P.parserType (queryRootFromFields emptyIntro))
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
  pure $ P.selectionSet queryRoot Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
buildQueryParser
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
  -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
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
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => [P.FieldParser n (QueryRootField UnpreparedValue)]
  -> [ActionInfo]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
buildSubscriptionParser pgQueryFields allActions = do
  actionSubscriptionFields <- buildActionSubscriptionFields allActions
  let subscriptionFields = pgQueryFields <> actionSubscriptionFields
  pure $ P.selectionSet subscriptionRoot Nothing subscriptionFields
         <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

-- buildRelayQueryParser
--   :: forall m n r
--    . ( MonadSchema n m
--      , MonadTableInfo r m
--      , MonadRole r m
--      , Has QueryContext r
--      )
--   => [P.FieldParser n (QueryRootField UnpreparedValue)]
--   -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
--   -> Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
--   -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
-- buildRelayQueryParser queryFields mutationParser subscriptionParser = do
--   queryWithIntrospectionHelper queryFields mutationParser subscriptionParser

buildPGMutationFields
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => Scenario -> PGSourceConfig -> HashSet QualifiedTable
  -> m [P.FieldParser n (MutationRootField UnpreparedValue)]
buildPGMutationFields scenario sourceConfig allTables = do
  concat . catMaybes <$> for (toList allTables) \table -> do
    tableCoreInfo <- _tiCoreInfo <$> askTableInfo table
    displayName   <- qualifiedObjectToName table
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
        let insertName = $$(G.litName "insert_") <> displayName
            insertDesc = G.Description $ "insert data into the table: " <>> table
            insertOneName = $$(G.litName "insert_") <> displayName <> $$(G.litName "_one")
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
        let updateName = $$(G.litName "update_") <> displayName
            updateDesc = G.Description $ "update data of the table: " <>> table
            updateByPkName = $$(G.litName "update_") <> displayName <> $$(G.litName "_by_pk")
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
          delete <- buildDeleteField table displayName (_tcrfDelete customRootFields)
            deletePermission _permSel
          -- select permission is needed for deleteByPk field so that a return type
          -- for the field can be generated
          deleteByPk <- fmap join $ for _permSel $
            buildDeleteByPkField table displayName (_tcrfDeleteByPk customRootFields) deletePermission

          pure $ fmap (asDbRootField . MDBDelete) <$> delete : maybeToList deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]

  where
    buildDeleteField table displayName customName deletePermission selectPermission = do
      let deleteName = $$(G.litName "delete_") <> displayName
          deleteDesc = G.Description $ "delete data from the table: " <>> table
      deleteFromTable table (fromMaybe deleteName customName) (Just deleteDesc)
        deletePermission selectPermission

    buildDeleteByPkField table displayName customName deletePermission = do
      let fieldName = $$(G.litName "delete_") <> displayName <> $$(G.litName "_by_pk")
          fieldDescription = G.Description $ "delete single row from the table: " <>> table
      deleteFromTableByPk table (fromMaybe fieldName customName) (Just fieldDescription) deletePermission

    asDbRootField =
      let pgExecCtx = _pscExecCtx sourceConfig
      in RFDB pgExecCtx

subscriptionRoot :: G.Name
subscriptionRoot = $$(G.litName "subscription_root")

mutationRoot :: G.Name
mutationRoot = $$(G.litName "mutation_root")

queryRoot :: G.Name
queryRoot = $$(G.litName "query_root")

buildMutationParser
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> [P.FieldParser n (MutationRootField UnpreparedValue)]
  -> m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))))
buildMutationParser allRemotes allActions nonObjectCustomTypes pgMutationFields = do

  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionSynchronous ->
        fmap (fmap (RFAction . AMSync)) <$> actionExecute nonObjectCustomTypes actionInfo
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation nonObjectCustomTypes actionInfo
      ActionQuery -> pure Nothing

  let mutationFieldsParser = pgMutationFields <> catMaybes actionParsers <> fmap (fmap RFRemote) allRemotes
  pure if null mutationFieldsParser
       then Nothing
       else Just $ P.selectionSet mutationRoot (Just $ G.Description "mutation root") mutationFieldsParser
            <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))
