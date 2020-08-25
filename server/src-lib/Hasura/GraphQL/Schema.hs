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
     , TableCache
     , FunctionCache
     , HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)
     , ActionCache
     , NonObjectTypeMap
     )
     `arr`
     ( HashMap RoleName (RoleContext GQLContext)
     , GQLContext
     )
buildGQLContext =
  proc (queryType, allTables, allFunctions, allRemoteSchemas, allActions, nonObjectCustomTypes) -> do

    -- Scroll down a few pages for the actual body...

    let allRoles = Set.insert adminRoleName $
             (allTables ^.. folded.tiRolePermInfoMap.to Map.keys.folded)
          <> (allActionInfos ^.. folded.aiPermissions.to Map.keys.folded)

        tableFilter    = not . isSystemDefined . _tciSystemDefined
        functionFilter = not . isSystemDefined . fiSystemDefined

        validTables = Map.filter (tableFilter . _tiCoreInfo) allTables
        validFunctions = Map.elems $ Map.filter functionFilter allFunctions

        allActionInfos = Map.elems allActions
        queryRemotesMap =
          fmap (map fDefinition . piQuery . rscParsed . fst) allRemoteSchemas
        buildFullestDBSchema
          :: m ( Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue))
               , Maybe (Parser 'Output (P.ParseT Identity) (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
               )
        buildFullestDBSchema = do
          SQLGenCtx{ stringifyNum } <- askSQLGenCtx
          let gqlContext =
                (,)
                <$> queryWithIntrospection (Set.fromMap $ validTables $> ())
                      validFunctions mempty mempty
                      allActionInfos nonObjectCustomTypes
                <*> mutation (Set.fromMap $ validTables $> ()) mempty
                      allActionInfos nonObjectCustomTypes
          flip runReaderT (adminRoleName, validTables, Frontend, QueryContext stringifyNum queryType queryRemotesMap) $
            P.runSchemaT gqlContext

    -- build the admin context so that we can check against name clashes with remotes
    adminHasuraContext <- bindA -< buildFullestDBSchema

    queryFieldNames :: [G.Name] <- bindA -<
      case P.discardNullability $ P.parserType $ fst adminHasuraContext of
        -- It really ought to be this case; anything else is a programming error.
        P.TNamed (P.Definition _ _ _ (P.TIObject (P.ObjectInfo rootFields _interfaces))) ->
          pure $ fmap P.dName rootFields
        _ -> throw500 "We encountered an root query of unexpected GraphQL type.  It should be an object type."
    let mutationFieldNames :: [G.Name]
        mutationFieldNames =
          case P.discardNullability . P.parserType <$> snd adminHasuraContext of
            Just (P.TNamed def) ->
              case P.dInfo def of
                -- It really ought to be this case; anything else is a programming error.
                P.TIObject (P.ObjectInfo rootFields _interfaces) -> fmap P.dName rootFields
                _                                                -> []
            _ -> []

    -- This block of code checks that there are no conflicting root field names between remotes.
    remotes ::
      [ ( RemoteSchemaName
        , ParsedIntrospection
        )
      ] <- (| foldlA' (\okSchemas (newSchemaName, (newSchemaContext, newMetadataObject)) -> do
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
                     returnA -< ())
                  |) newMetadataObject
                case checkedDuplicates of
                  Nothing -> returnA -< okSchemas
                  Just _  -> returnA -< (newSchemaName, rscParsed newSchemaContext):okSchemas
              ) |) [] (Map.toList allRemoteSchemas)

    let unauthenticatedContext :: m GQLContext
        unauthenticatedContext = do
          let gqlContext = GQLContext . finalizeParser <$>
                unauthenticatedQueryWithIntrospection queryRemotes mutationRemotes
          halfContext <- P.runSchemaT gqlContext
          return $ halfContext $ finalizeParser <$> unauthenticatedMutation mutationRemotes

        -- | The 'query' type of the remotes. TODO: also expose mutation
        -- remotes. NOT TODO: subscriptions, as we do not yet aim to support
        -- these.
        queryRemotes = concatMap (piQuery . snd) remotes
        mutationRemotes = concatMap (concat . piMutation . snd) remotes
        queryHasuraOrRelay = case queryType of
          QueryHasura -> queryWithIntrospection (Set.fromMap $ validTables $> ())
                         validFunctions queryRemotes mutationRemotes
                         allActionInfos nonObjectCustomTypes
          QueryRelay  -> relayWithIntrospection (Set.fromMap $ validTables $> ()) validFunctions

        buildContextForRoleAndScenario :: RoleName -> Scenario -> m GQLContext
        buildContextForRoleAndScenario roleName scenario = do
          SQLGenCtx{ stringifyNum } <- askSQLGenCtx
          let gqlContext = GQLContext
                <$> (finalizeParser <$> queryHasuraOrRelay)
                <*> (fmap finalizeParser <$> mutation (Set.fromList $ Map.keys validTables) mutationRemotes
                     allActionInfos nonObjectCustomTypes)
          flip runReaderT (roleName, validTables, scenario, QueryContext stringifyNum queryType queryRemotesMap) $
            P.runSchemaT gqlContext

        buildContextForRole :: RoleName -> m (RoleContext GQLContext)
        buildContextForRole roleName = do
          frontend <- buildContextForRoleAndScenario roleName Frontend
          backend <- buildContextForRoleAndScenario roleName Backend
          return $ RoleContext frontend $ Just backend

        finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
        finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

    -- Here, finally the body starts.

    roleContexts <- bindA -< (Set.toMap allRoles & Map.traverseWithKey \roleName () ->
                        buildContextForRole roleName)
    unauthenticated <- bindA -< unauthenticatedContext
    returnA -< (roleContexts, unauthenticated)

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
      displayName <- qualifiedObjectToName table
      let fieldsDesc = G.Description $ "fetch data from the table: " <>> table
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "fetch aggregated fields from the table: " <>> table
          pkName = displayName <> $$(G.litName "_by_pk")
          pkDesc = G.Description $ "fetch data from the table: " <> table <<> " using primary key columns"
      catMaybes <$> sequenceA
        [ requiredFieldParser (RFDB . QDBSimple)      $ selectTable          table (fromMaybe displayName $ _tcrfSelect          customRootFields) (Just fieldsDesc) perms
        , mapMaybeFieldParser (RFDB . QDBPrimaryKey)  $ selectTableByPk      table (fromMaybe pkName      $ _tcrfSelectByPk      customRootFields) (Just pkDesc)     perms
        , mapMaybeFieldParser (RFDB . QDBAggregation) $ selectTableAggregate table (fromMaybe aggName     $ _tcrfSelectAggregate customRootFields) (Just aggDesc)    perms
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
        [ requiredFieldParser (RFDB . QDBSimple)      $ selectFunction          function displayName (Just functionDesc) perms
        , mapMaybeFieldParser (RFDB . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms
        ]
  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionSynchronous -> pure Nothing
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo
      ActionQuery ->
        fmap (fmap (RFAction . AQQuery)) <$> actionExecute nonObjectCustomTypes actionInfo
  pure $ (concat . catMaybes) (tableSelectExpParsers <> functionSelectExpParsers <> toRemoteFieldParser allRemotes)
         <> catMaybes actionParsers
  where
    requiredFieldParser :: (a -> b) -> m (P.FieldParser n a) -> m (Maybe (P.FieldParser n b))
    requiredFieldParser f = fmap $ Just . fmap f

    mapMaybeFieldParser :: (a -> b) -> m (Maybe (P.FieldParser n a)) -> m (Maybe (P.FieldParser n b))
    mapMaybeFieldParser f = fmap $ fmap $ fmap f

    toRemoteFieldParser p = [Just $ fmap (fmap RFRemote) p]

-- | Similar to @query'@ but for Relay.
relayQuery'
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
relayQuery' allTables allFunctions = do
  tableConnectionSelectParsers <-
    for (toList allTables) $ \table -> runMaybeT do
      pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                     <$> askTableInfo table
      selectPerms <- MaybeT $ tableSelectPermissions table
      displayName <- qualifiedObjectToName table
      let fieldName = displayName <> $$(G.litName "_connection")
          fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> table
      lift $ selectTableConnection table fieldName fieldDesc pkeyColumns selectPerms

  functionConnectionSelectParsers <-
    for allFunctions $ \function -> runMaybeT do
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

  pure $ map ((RFDB . QDBConnection) <$>) $ catMaybes $
         tableConnectionSelectParsers <> functionConnectionSelectParsers

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
        , sDirectives = defaultDirectives
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

-- | Prepare the parser for query-type GraphQL requests, but with introspection
--   for queries, mutations and subscriptions built in.
queryWithIntrospection
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
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
  queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP

relayWithIntrospection
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has Scenario r
     )
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
relayWithIntrospection allTables allFunctions = do
  nodeFP <- fmap (RFDB . QDBPrimaryKey) <$> nodeField
  basicQueryFP <- relayQuery' allTables allFunctions
  mutationP <- mutation allTables [] [] mempty
  let relayQueryFP = nodeFP:basicQueryFP
      subscriptionP = P.selectionSet $$(G.litName "subscription_root") Nothing relayQueryFP
                      <&> fmap (P.handleTypename (RFRaw . J.String. G.unName))
      basicQueryP = queryRootFromFields relayQueryFP
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
        , sDirectives = defaultDirectives
        }
  let partialQueryFields =
        nodeFP : basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

-- | Prepare the parser for query-type GraphQL requests, but with introspection
-- for queries, mutations and subscriptions built in.
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
  queryWithIntrospectionHelper basicQueryFP mutationP subscriptionP

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
    tableCoreInfo <- _tiCoreInfo <$> askTableInfo table
    displayName   <- qualifiedObjectToName table
    tablePerms    <- tablePermissions table
    for tablePerms \permissions -> do
      let customRootFields = _tcCustomRootFields $ _tciCustomConfig tableCoreInfo
          viewInfo         = _tciViewInfo tableCoreInfo
          selectPerms      = _permSel permissions

      -- If we're in a frontend scenario, we should not include backend_only inserts
      scenario <- asks getter
      let scenarioInsertPermissionM = do
            insertPermission <- _permIns permissions
            if scenario == Frontend && ipiBackendOnly insertPermission
              then Nothing
              else return insertPermission
      inserts <- fmap join $ whenMaybe (isMutable viIsInsertable viewInfo) $ for scenarioInsertPermissionM \insertPerms -> do
        let insertName = $$(G.litName "insert_") <> displayName
            insertDesc = G.Description $ "insert data into the table: " <>> table
            insertOneName = $$(G.litName "insert_") <> displayName <> $$(G.litName "_one")
            insertOneDesc = G.Description $ "insert a single row into the table: " <>> table
        insert <- insertIntoTable table (fromMaybe insertName $ _tcrfInsert customRootFields) (Just insertDesc) insertPerms selectPerms (_permUpd permissions)
        -- select permissions are required for InsertOne: the
        -- selection set is the same as a select on that table, and it
        -- therefore can't be populated if the user doesn't have
        -- select permissions
        insertOne <- for selectPerms \selPerms ->
          insertOneIntoTable table (fromMaybe insertOneName $ _tcrfInsertOne customRootFields) (Just insertOneDesc) insertPerms selPerms (_permUpd permissions)
        pure $ fmap (RFDB . MDBInsert) insert : maybe [] (pure . fmap (RFDB . MDBInsert)) insertOne

      updates <- fmap join $ whenMaybe (isMutable viIsUpdatable viewInfo) $ for (_permUpd permissions) \updatePerms -> do
        let updateName = $$(G.litName "update_") <> displayName
            updateDesc = G.Description $ "update data of the table: " <>> table
            updateByPkName = $$(G.litName "update_") <> displayName <> $$(G.litName "_by_pk")
            updateByPkDesc = G.Description $ "update single row of the table: " <>> table
        update <- updateTable table (fromMaybe updateName $ _tcrfUpdate customRootFields) (Just updateDesc) updatePerms selectPerms
        -- likewise; furthermore, primary keys can only be tested in
        -- the `where` clause if the user has select permissions for
        -- them, which at the very least requires select permissions
        updateByPk <- join <$> for selectPerms
          (updateTableByPk table (fromMaybe updateByPkName $ _tcrfUpdateByPk customRootFields) (Just updateByPkDesc) updatePerms)
        pure $ fmap (RFDB . MDBUpdate) <$> catMaybes [update, updateByPk]

      deletes <- fmap join $ whenMaybe (isMutable viIsDeletable viewInfo) $ for (_permDel permissions) \deletePerms -> do
        let deleteName = $$(G.litName "delete_") <> displayName
            deleteDesc = G.Description $ "delete data from the table: " <>> table
            deleteByPkName = $$(G.litName "delete_") <> displayName <> $$(G.litName "_by_pk")
            deleteByPkDesc = G.Description $ "delete single row from the table: " <>> table
        delete <- deleteFromTable table (fromMaybe deleteName $ _tcrfDelete customRootFields) (Just deleteDesc) deletePerms selectPerms

        -- ditto
        deleteByPk <- join <$> for selectPerms
          (deleteFromTableByPk table (fromMaybe deleteByPkName $ _tcrfDeleteByPk customRootFields) (Just deleteByPkDesc) deletePerms)
        pure $ fmap (RFDB . MDBDelete) delete : maybe [] (pure . fmap (RFDB . MDBDelete)) deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]

  actionParsers <- for allActions $ \actionInfo ->
    case _adType (_aiDefinition actionInfo) of
      ActionMutation ActionSynchronous ->
        fmap (fmap (RFAction . AMSync)) <$> actionExecute nonObjectCustomTypes actionInfo
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation nonObjectCustomTypes actionInfo
      ActionQuery -> pure Nothing

  let mutationFieldsParser = concat (catMaybes mutationParsers) <> catMaybes actionParsers <> fmap (fmap RFRemote) allRemotes
  pure if null mutationFieldsParser
       then Nothing
       else Just $ P.selectionSet $$(G.litName "mutation_root") (Just $ G.Description "mutation root") mutationFieldsParser
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
