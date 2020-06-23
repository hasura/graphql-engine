module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.Extended          as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as S
import qualified Language.GraphQL.Draft.Syntax         as G

import           Control.Lens.Extended
import           Control.Monad.Unique

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser                 (Kind (..), Parser, Schema (..),
                                                        UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Introspect
import           Hasura.GraphQL.Schema.Mutation
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types

buildGQLContext
  :: forall m
   . ( MonadIO m -- see Note [SchemaT requires MonadIO] in Hasura.GraphQL.Parser.Monad
     , MonadUnique m
     , MonadError QErr m
     , HasSQLGenCtx m )
  => TableCache
  -> FunctionCache
  -> HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject)
  -> m (HashMap RoleName GQLContext)
buildGQLContext allTables allFunctions allRemoteSchemata =
  S.toMap allRoles & Map.traverseWithKey \roleName () ->
    buildContextForRole roleName
  where
    allRoles :: HashSet RoleName
    allRoles = S.insert adminRole $ allTables ^.. folded.tiRolePermInfoMap.to Map.keys.folded

    tableFilter ti = not (isSystemDefined $ _tciSystemDefined ti)
    functionFilter = not . isSystemDefined . fiSystemDefined

    validTables = Map.filter (tableFilter . _tiCoreInfo) allTables
    validFunctions = Map.elems $ Map.filter functionFilter allFunctions

    allRemoteParsers ::
      m (HashMap RemoteSchemaName
         ( [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
         , Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
         , Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]))
    allRemoteParsers = P.runSchemaT _roleName _allTables do
      traverse (buildRemoteParser . fst) allRemoteSchemata

    buildContextForRole roleName = do
      SQLGenCtx{ stringifyNum } <- askSQLGenCtx
      remotes <- allRemoteParsers
      let queryRemotes = concat $ map (\(q,m,s)->q) $ Map.elems remotes
      P.runSchemaT roleName allTables $ GQLContext
        <$> (finalizeParser <$> queryWithIntrospection (S.fromList $ Map.keys validTables) validFunctions queryRemotes stringifyNum)
        <*> (finalizeParser <$> mutation (S.fromList $ Map.keys validTables) stringifyNum)

    finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
    finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

-- | Generate all the field parsers for query-type GraphQL requests.  We don't
-- actually collect these into a @Parser@ using @selectionSet@ so that we can
-- insert the introspection before doing so.
query'
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> Bool
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
query' allTables allFunctions allRemotes stringifyNum = do
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
        [ toQrf2 (RFDB . QDBSimple)      $ selectTable          table (fromMaybe displayName $ _tcrfSelect          customRootFields) (Just fieldsDesc) perms stringifyNum
        , toQrf3 (RFDB . QDBPrimaryKey)  $ selectTableByPk      table (fromMaybe pkName      $ _tcrfSelectByPk      customRootFields) (Just pkDesc)     perms stringifyNum
        , toQrf3 (RFDB . QDBAggregation) $ selectTableAggregate table (fromMaybe aggName     $ _tcrfSelectAggregate customRootFields) (Just aggDesc)    perms stringifyNum
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
        [ toQrf2 (RFDB . QDBSimple)      $ selectFunction          function displayName (Just functionDesc) perms stringifyNum
        , toQrf3 (RFDB . QDBAggregation) $ selectFunctionAggregate function aggName     (Just aggDesc)      perms stringifyNum
        ]
  pure $ concat $ catMaybes $ tableSelectExpParsers <> functionSelectExpParsers <> conv allRemotes
  where
    -- TODO: this is a terrible name, there must be something better
    toQrf2 :: (a -> b) -> m (P.FieldParser n a) -> m (Maybe (P.FieldParser n b))
    toQrf2 f = fmap $ Just . fmap f
    toQrf3 f = fmap $ fmap $ fmap f
    conv fps = [Just $ fmap (fmap RFRemote) fps]

-- | Parse query-type GraphQL requests without introspection
query
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => G.Name
  -> HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
query name allTables allFunctions allRemotes stringifyNum = do
  queryFieldsParser <- query' allTables allFunctions allRemotes stringifyNum
  pure $ P.selectionSet name Nothing queryFieldsParser
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

subscription
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
subscription allTables allFunctions stringifyNum =
  query $$(G.litName "subscription_root") allTables allFunctions [] stringifyNum


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
  introspectionTypes <- collectTypes (P.pType emptyQueryP)
  let introspectionSchema = Schema
        { sDescription = Nothing
        , sTypes = mempty
        , sQueryType = P.pType (emptyQueryP)
        , sMutationType = Nothing
        , sSubscriptionType = Nothing
        , sDirectives = mempty
        }
  return $ fmap (fmap RFRaw) [schema introspectionSchema, typeIntrospection introspectionSchema]

collectTypes
  :: forall m
   . MonadError QErr m
  => P.Type 'Output
  -> m (HashMap G.Name (P.Definition P.SomeTypeInfo))
collectTypes tp = case P.collectTypeDefinitions tp of
  Left (P.ConflictingDefinitions type1 _) -> throw500 $
    "found conflicting definitions for " <> P.getName type1
    <<> " when collecting types from the schema"
  Right tps -> pure tps

-- | Prepare the parser for query-type GraphQL requests, but with introspection
-- for queries, mutations and subscriptions (TODO) built in.
queryWithIntrospection
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> [FunctionInfo]
  -> [P.FieldParser n (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospection allTables allFunctions allRemotes stringifyNum = do
  basicQueryFP <- query' allTables allFunctions allRemotes stringifyNum
  mutationP <- mutation allTables stringifyNum
  subscriptionP <- subscription allTables allFunctions stringifyNum
  let
    basicQueryP = queryRootFromFields basicQueryFP
  allQueryTypes <- collectTypes (P.pType basicQueryP)
  allMutationTypes <- collectTypes (P.pType mutationP)
  allSubscriptionTypes <- collectTypes (P.pType subscriptionP)
  emptyIntro <- emptyIntrospection
  allIntrospectionTypes <- collectTypes (P.pType (queryRootFromFields emptyIntro))
  let allTypes = Map.unions
        [ allQueryTypes
        , allMutationTypes
        , allSubscriptionTypes
        , Map.filterWithKey (\name _info -> name /= $$(G.litName "query_root")) allIntrospectionTypes
        ]
      partialSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = P.pType basicQueryP
        , sMutationType = Just $ P.pType mutationP
        -- TODO make sure NOT to expose remote schemata via subscription introspection (when remote schemata are implemented)
        , sSubscriptionType = Just $ P.pType subscriptionP
        , sDirectives = []
        }
  let partialQueryFields =
        basicQueryFP ++ (fmap RFRaw <$> [schema partialSchema, typeIntrospection partialSchema])
  pure $ P.selectionSet $$(G.litName "query_root") Nothing partialQueryFields
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))

mutation
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
mutation allTables stringifyNum = do
  mutationParsers <- for (toList allTables) \table -> do
    customRootFields <- _tcCustomRootFields . _tciCustomConfig . _tiCoreInfo <$> askTableInfo table
    displayName <- P.qualifiedObjectToName table
    tablePerms  <- tablePermissions table
    for tablePerms \permissions -> do
      let selectPerms = _permSel permissions

      inserts <- for (_permIns permissions) \insertPerms -> do
        let insertName = $$(G.litName "insert_") <> displayName
            insertDesc = G.Description $ "insert data into the table: \"" <> G.unName displayName <> "\""
            insertOneName = $$(G.litName "insert_") <> displayName <> $$(G.litName "_one")
            insertOneDesc = G.Description $ "insert a single row into the table: \"" <> G.unName displayName <> "\""
        insert <- insertIntoTable table (fromMaybe insertName $ _tcrfInsert customRootFields) (Just insertDesc) insertPerms selectPerms (_permUpd permissions) stringifyNum
        -- select permissions are required for InsertOne: the
        -- selection set is the same as a select on that table, and it
        -- therefore can't be populated if the user doesn't have
        -- select permissions
        insertOne <- for selectPerms \selPerms ->
          insertOneIntoTable table (fromMaybe insertOneName $ _tcrfInsertOne customRootFields) (Just insertOneDesc) insertPerms selPerms (_permUpd permissions) stringifyNum
        pure $ fmap (RFDB . MDBInsert) insert : maybe [] (pure . fmap (RFDB . MDBInsert)) insertOne

      updates <- for (_permUpd permissions) \updatePerms -> do
        let updateName = $$(G.litName "update_") <> displayName
            updateDesc = G.Description $ "update data of the table: \"" <> G.unName displayName <> "\""
            updateByPkName = $$(G.litName "update_") <> displayName <> $$(G.litName "_by_pk")
            updateByPkDesc = G.Description $ "update single row of the table: \"" <> G.unName displayName <> "\""
        update <- updateTable table (fromMaybe updateName $ _tcrfUpdate customRootFields) (Just updateDesc) updatePerms selectPerms stringifyNum
        -- likewise; furthermore, primary keys can only be tested in
        -- the `where` clause if the user has select permissions for
        -- them, which at the very least requires select permissions
        updateByPk <- join <$> for selectPerms \selPerms ->
          updateTableByPk table (fromMaybe updateByPkName $ _tcrfUpdateByPk customRootFields) (Just updateByPkDesc) updatePerms selPerms stringifyNum
        pure $ fmap (RFDB . MDBUpdate) <$> catMaybes [update, updateByPk]

      deletes <- for (_permDel permissions) \deletePerms -> do
        let deleteName = $$(G.litName "delete_") <> displayName
            deleteDesc = G.Description $ "delete data from the table: \"" <> G.unName displayName <> "\""
            deleteByPkName = $$(G.litName "delete_") <> displayName <> $$(G.litName "_by_pk")
            deleteByPkDesc = G.Description $ "delete single row from the table: \"" <> G.unName displayName <> "\""
        delete <- deleteFromTable table (fromMaybe deleteName $ _tcrfDelete customRootFields) (Just deleteDesc) deletePerms selectPerms stringifyNum
        -- ditto
        deleteByPk <- join <$> for selectPerms \selPerms ->
          deleteFromTableByPk table (fromMaybe deleteByPkName $ _tcrfDeleteByPk customRootFields) (Just deleteByPkDesc) deletePerms selPerms stringifyNum
        pure $ fmap (RFDB . MDBDelete) delete : maybe [] (pure . fmap (RFDB . MDBDelete)) deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]
  let mutationFieldsParser = concat $ catMaybes mutationParsers
  pure $ P.selectionSet $$(G.litName "mutation_root") Nothing mutationFieldsParser
    <&> fmap (P.handleTypename (RFRaw . J.String . G.unName))
