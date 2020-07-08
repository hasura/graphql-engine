module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as S
import qualified Language.GraphQL.Draft.Syntax         as G

import           Control.Lens.Extended
import           Control.Monad.Unique
import           Data.Has

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
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

-- TODO So far, we're *always* generating a query_root, mutation_root and
-- subscription_root.  But is this valid?  We may end up generating objects with
-- 0 (exposed) fields.  Perhaps we should be a bit more careful.  I suppose
-- `query` always exists because `__typename` is exposed... right?

buildGQLContext
  :: forall m
   . ( MonadIO m -- see Note [SchemaT requires MonadIO] in Hasura.GraphQL.Parser.Monad
     , MonadUnique m
     , MonadError QErr m
     , HasSQLGenCtx m )
  => GraphQLQueryType
  -> TableCache
  -> FunctionCache
  -> HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)
  -> ActionCache
  -> NonObjectTypeMap
  -> m (HashMap RoleName (RoleContext GQLContext), GQLContext)
buildGQLContext queryType allTables allFunctions allRemoteSchemas allActions nonObjectCustomTypes = do
  roleContexts <- (S.toMap allRoles & Map.traverseWithKey \roleName () ->
                      buildContextForRole roleName)
  unauthenticated <- unauthenticatedContext
  return (roleContexts, unauthenticated)
  where
    allRoles :: HashSet RoleName
    allRoles = S.insert adminRoleName $ allTables ^.. folded.tiRolePermInfoMap.to Map.keys.folded

    tableFilter    = not . isSystemDefined . _tciSystemDefined
    functionFilter = not . isSystemDefined . fiSystemDefined

    validTables = Map.filter (tableFilter . _tiCoreInfo) allTables
    validFunctions = Map.elems $ Map.filter functionFilter allFunctions

    allRemoteParsers ::
      m (HashMap RemoteSchemaName
         ( [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
         , Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
         , Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]))
    allRemoteParsers = P.runSchemaT $ traverse (buildRemoteParser . fst) allRemoteSchemas

    unauthenticatedContext :: m GQLContext
    unauthenticatedContext = do
      remotes <- allRemoteParsers
      let gqlContext = GQLContext . finalizeParser <$>
            unauthenticatedQueryWithIntrospection (queryRemotes remotes) (mutationRemotes remotes)
      halfContext <- P.runSchemaT gqlContext
      return $ halfContext $ finalizeParser <$> (unauthenticatedMutation (mutationRemotes remotes))

    -- | The 'query' type of the remotes. TODO: also expose mutation
    -- remotes. NOT TODO: subscriptions, as we do not yet aim to support
    -- these.
    queryRemotes remotes = concat $ map (\(q,_,_)->q) $ Map.elems remotes
    queryRemotesMap remotes =
      Map.fromList $
      map (\(remoteSchemaName, (queryFieldParser, _, _) ) ->
             (remoteSchemaName, map (\(FieldParser defn _) -> defn) queryFieldParser))
      $ Map.toList remotes
    mutationRemotes remotes = concat $ fmap concat $ map (\(_,m,_)->m) $ Map.elems remotes
    allActionInfos = Map.elems allActions
    queryHasuraOrRelay remotes = case queryType of
      QueryHasura -> queryWithIntrospection (S.fromList $ Map.keys validTables)
                     validFunctions (queryRemotes remotes) (mutationRemotes remotes)
                     allActionInfos nonObjectCustomTypes
      QueryRelay  -> relayWithIntrospection (S.fromList $ Map.keys validTables)

    buildContextForRole :: RoleName -> m (RoleContext GQLContext)
    buildContextForRole roleName = do
      frontend <- buildContextForRoleAndScenario roleName Frontend
      backend <- buildContextForRoleAndScenario roleName Backend
      return $ RoleContext frontend $ Just backend

    buildContextForRoleAndScenario :: RoleName -> Scenario -> m GQLContext
    buildContextForRoleAndScenario roleName scenario = do
      SQLGenCtx{ stringifyNum } <- askSQLGenCtx
      remotes <- allRemoteParsers
      let gqlContext = GQLContext
            <$> (finalizeParser <$> queryHasuraOrRelay remotes)
            <*> (fmap (fmap finalizeParser) $ mutation (S.fromList $ Map.keys validTables) (mutationRemotes remotes)
                 allActionInfos nonObjectCustomTypes)
      flip runReaderT (queryType, roleName, allTables, scenario, QueryContext stringifyNum (queryRemotesMap remotes)) $
        P.runSchemaT gqlContext

    finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
    finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

-- | Generate all the field parsers for query-type GraphQL requests.  We don't
-- actually collect these into a @Parser@ using @selectionSet@ so that we can
-- insert the introspection before doing so.
query'
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
query' allTables allFunctions allRemotes allActions nonObjectCustomTypes = do
  tableSelectExpParsers <- for (toList allTables) \table -> do
    selectPerms <- tableSelectPermissions table
    customRootFields <- _tcCustomRootFields . _tciCustomConfig . _tiCoreInfo <$> askTableInfo table
    for selectPerms \perms -> do
      displayName <- P.qualifiedObjectToName table
      let fieldsDesc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName table) <> "\""
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "fetch aggregated fields from the table: \"" <> getTableTxt (qName table) <> "\""
          pkName = displayName <> $$(G.litName "_by_pk")
          pkDesc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName table) <> "\" using primary key columns"
      catMaybes <$> sequenceA
        [ toQrf2 (RFDB . QDBSimple)      $ selectTable          table (fromMaybe displayName $ _tcrfSelect          customRootFields) (Just fieldsDesc) perms
        , toQrf3 (RFDB . QDBPrimaryKey)  $ selectTableByPk      table (fromMaybe pkName      $ _tcrfSelectByPk      customRootFields) (Just pkDesc)     perms
        , toQrf3 (RFDB . QDBAggregation) $ selectTableAggregate table (fromMaybe aggName     $ _tcrfSelectAggregate customRootFields) (Just aggDesc)    perms
        ]
  functionSelectExpParsers <- for allFunctions \function -> do
    let targetTable = fiReturnType function
    selectPerms <- tableSelectPermissions targetTable
    for selectPerms \perms -> do
      displayName <- P.qualifiedObjectToName $ fiName function
      let tableName = getTableTxt $ qName targetTable
          functionName = getFunctionTxt $ qName $ fiName function
          functionDesc = G.Description $ "execute function \"" <> functionName <> "\" which returns \"" <> tableName <> "\""
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "execute function \"" <> functionName <> "\" and query aggregates on result of table type \"" <> tableName <> "\""
      catMaybes <$> sequenceA
        [ toQrf2 (RFDB . QDBSimple)      $ selectFunction          function displayName (Just functionDesc) perms
        , toQrf3 (RFDB . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms
        ]
  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation (ActionSynchronous) -> pure Nothing
      ActionMutation (ActionAsynchronous) ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo
      ActionQuery ->
        fmap (fmap (RFAction . AQQuery)) <$>
        actionExecute nonObjectCustomTypes actionInfo
  pure $ (concat . catMaybes) (tableSelectExpParsers <> functionSelectExpParsers <> conv allRemotes)
         <> catMaybes actionParsers
  where
    -- TODO: this is a terrible name, there must be something better
    toQrf2 :: (a -> b) -> m (P.FieldParser n a) -> m (Maybe (P.FieldParser n b))
    toQrf2 f = fmap $ Just . fmap f
    toQrf3 f = fmap $ fmap $ fmap f
    conv fps = [Just $ fmap (fmap RFRemote) fps]

-- | Parse query-type GraphQL requests without introspection
query
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => G.Name
  -> HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
query name allTables allFunctions allRemotes allActions nonObjectCustomTypes = do
  queryFieldsParser <- query' allTables allFunctions allRemotes allActions nonObjectCustomTypes
  pure $ P.selectionSet name Nothing queryFieldsParser
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

subscription
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> [ActionInfo]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
subscription allTables allFunctions asyncActions =
  query $$(G.litName "subscription_root") allTables allFunctions [] asyncActions mempty

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
        , sQueryType = P.parserType (emptyQueryP)
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

-- | Prepare the parser for query-type GraphQL requests, but with introspection
-- for queries, mutations and subscriptions (TODO) built in.
queryWithIntrospection
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     -- This context is here because we'll probably need it in Select.hs at some point
     , Has GraphQLQueryType r
     , Has Scenario r
     )
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospection allTables allFunctions queryRemotes mutationRemotes allActions nonObjectCustomTypes = do
  basicQueryFP <- query' allTables allFunctions queryRemotes allActions nonObjectCustomTypes
  mutationP <- mutation allTables mutationRemotes allActions nonObjectCustomTypes
  subscriptionP <- subscription allTables allFunctions $
                   filter (has (aiDefinition.adType._ActionMutation._ActionAsynchronous)) allActions
  let
    basicQueryP = queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
  allBasicTypes <- collectTypes $
    [ P.parserType basicQueryP
    , P.parserType subscriptionP
    ]
    ++ maybeToList (P.parserType <$> mutationP)
  allIntrospectionTypes <- collectTypes (P.parserType (queryRootFromFields emptyIntro))
  let allTypes = Map.unions
        [ allBasicTypes
        , Map.filterWithKey (\name _info -> name /= $$(G.litName "query_root")) allIntrospectionTypes
        ]
      partialSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = P.parserType basicQueryP
        , sMutationType = P.parserType <$> mutationP
        , sSubscriptionType = Just $ P.parserType subscriptionP
        , sDirectives = []
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

relayWithIntrospection
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     -- This context is here because we'll probably need it in Select.hs at some point
     , Has GraphQLQueryType r
     , Has Scenario r
     )
  => HashSet QualifiedTable
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
relayWithIntrospection allTables = do
  nodeF <- nodeField allTables
  mutationP <- mutation allTables [] [] mempty
  let basicQueryFP = (:[]) . (fmap (RFDB . QDBPrimaryKey)) $ nodeF
      basicQueryP = queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
  allBasicTypes <- collectTypes $
    [ P.parserType basicQueryP
    ]
    ++ maybeToList (P.parserType <$> mutationP)
  allIntrospectionTypes <- collectTypes (P.parserType (queryRootFromFields emptyIntro))
  let allTypes = Map.unions
        [ allBasicTypes
        , Map.filterWithKey (\name _info -> name /= $$(G.litName "query_root")) allIntrospectionTypes
        ]
      partialSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = P.parserType basicQueryP
        , sMutationType = P.parserType <$> mutationP
        , sSubscriptionType = Nothing
        , sDirectives = []
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

-- | Prepare the parser for query-type GraphQL requests, but with introspection
-- for queries, mutations and subscriptions (TODO) built in.
unauthenticatedQueryWithIntrospection
  :: forall m n
   . ( MonadSchema n m
     , MonadError QErr m
     )
  => [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
unauthenticatedQueryWithIntrospection queryRemotes mutationRemotes = do
  let basicQueryFP = fmap (fmap RFRemote) queryRemotes
      mutationP = unauthenticatedMutation mutationRemotes
      subscriptionP = unauthenticatedSubscription @n
      basicQueryP = queryRootFromFields basicQueryFP
  emptyIntro <- emptyIntrospection
  allBasicTypes <- collectTypes $
    [ P.parserType basicQueryP
    , P.parserType subscriptionP
    ]
    ++ maybeToList (P.parserType <$> mutationP)
  allIntrospectionTypes <- collectTypes (P.parserType (queryRootFromFields emptyIntro))
  let allTypes = Map.unions
        [ allBasicTypes
        , Map.filterWithKey (\name _info -> name /= $$(G.litName "query_root")) allIntrospectionTypes
        ]
      partialSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = P.parserType basicQueryP
        , sMutationType = P.parserType <$> mutationP
        , sSubscriptionType = Just $ P.parserType subscriptionP
        , sDirectives = []
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

mutation
  :: forall m n r
   . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r, Has Scenario r)
  => HashSet QualifiedTable
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> [ActionInfo]
  -> NonObjectTypeMap
  -> m (Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue))))
mutation allTables allRemotes allActions nonObjectCustomTypes = do
  mutationParsers <- for (toList allTables) \table -> do
    customRootFields <- _tcCustomRootFields . _tciCustomConfig . _tiCoreInfo <$> askTableInfo table
    displayName <- P.qualifiedObjectToName table
    tablePerms  <- tablePermissions table
    for tablePerms \permissions -> do
      let selectPerms = _permSel permissions

      -- If we're in a frontend scenario, we should not include backend_only inserts
      scenario <- asks getter
      let scenarioInsertPermissionM = do
            insertPermission <- _permIns permissions
            if (scenario == Frontend && ipiBackendOnly insertPermission)
              then Nothing
              else return insertPermission
      inserts <- for scenarioInsertPermissionM \insertPerms -> do
        let insertName = $$(G.litName "insert_") <> displayName
            insertDesc = G.Description $ "insert data into the table: \"" <> G.unName displayName <> "\""
            insertOneName = $$(G.litName "insert_") <> displayName <> $$(G.litName "_one")
            insertOneDesc = G.Description $ "insert a single row into the table: \"" <> G.unName displayName <> "\""
        insert <- insertIntoTable table (fromMaybe insertName $ _tcrfInsert customRootFields) (Just insertDesc) insertPerms selectPerms (_permUpd permissions)
        -- select permissions are required for InsertOne: the
        -- selection set is the same as a select on that table, and it
        -- therefore can't be populated if the user doesn't have
        -- select permissions
        insertOne <- for selectPerms \selPerms ->
          insertOneIntoTable table (fromMaybe insertOneName $ _tcrfInsertOne customRootFields) (Just insertOneDesc) insertPerms selPerms (_permUpd permissions)
        pure $ fmap (RFDB . MDBInsert) insert : maybe [] (pure . fmap (RFDB . MDBInsert)) insertOne

      updates <- for (_permUpd permissions) \updatePerms -> do
        let updateName = $$(G.litName "update_") <> displayName
            updateDesc = G.Description $ "update data of the table: \"" <> G.unName displayName <> "\""
            updateByPkName = $$(G.litName "update_") <> displayName <> $$(G.litName "_by_pk")
            updateByPkDesc = G.Description $ "update single row of the table: \"" <> G.unName displayName <> "\""
        update <- updateTable table (fromMaybe updateName $ _tcrfUpdate customRootFields) (Just updateDesc) updatePerms selectPerms
        -- likewise; furthermore, primary keys can only be tested in
        -- the `where` clause if the user has select permissions for
        -- them, which at the very least requires select permissions
        updateByPk <- join <$> for selectPerms \selPerms ->
          updateTableByPk table (fromMaybe updateByPkName $ _tcrfUpdateByPk customRootFields) (Just updateByPkDesc) updatePerms selPerms
        pure $ fmap (RFDB . MDBUpdate) <$> catMaybes [update, updateByPk]

      deletes <- for (_permDel permissions) \deletePerms -> do
        let deleteName = $$(G.litName "delete_") <> displayName
            deleteDesc = G.Description $ "delete data from the table: \"" <> G.unName displayName <> "\""
            deleteByPkName = $$(G.litName "delete_") <> displayName <> $$(G.litName "_by_pk")
            deleteByPkDesc = G.Description $ "delete single row from the table: \"" <> G.unName displayName <> "\""
        delete <- deleteFromTable table (fromMaybe deleteName $ _tcrfDelete customRootFields) (Just deleteDesc) deletePerms selectPerms
        -- ditto
        deleteByPk <- join <$> for selectPerms \selPerms ->
          deleteFromTableByPk table (fromMaybe deleteByPkName $ _tcrfDeleteByPk customRootFields) (Just deleteByPkDesc) deletePerms selPerms
        pure $ fmap (RFDB . MDBDelete) delete : maybe [] (pure . fmap (RFDB . MDBDelete)) deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]

  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation (ActionSynchronous) ->
        fmap (fmap (RFAction . AMSync)) <$>
        actionExecute nonObjectCustomTypes actionInfo
      ActionMutation (ActionAsynchronous) ->
        fmap (fmap (RFAction . AMAsync)) <$>
        actionAsyncMutation nonObjectCustomTypes actionInfo
      ActionQuery -> pure Nothing

  let mutationFieldsParser = concat (catMaybes mutationParsers) <> catMaybes actionParsers <> (fmap (fmap RFRemote)) allRemotes
  pure if null mutationFieldsParser
       then Nothing
       else Just $ P.selectionSet $$(G.litName "mutation_root") Nothing mutationFieldsParser
            <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

unauthenticatedMutation
  :: forall n
   . MonadParse n
  => [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> Maybe (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
unauthenticatedMutation allRemotes =
  let mutationFieldsParser = fmap (fmap RFRemote) allRemotes
  in if null mutationFieldsParser
     then Nothing
     else Just $ P.selectionSet $$(G.litName "mutation_root") Nothing mutationFieldsParser
          <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

unauthenticatedSubscription
  :: forall n
   . MonadParse n
  => Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
unauthenticatedSubscription =
 P.selectionSet $$(G.litName "subscription_root") Nothing []
 <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))
