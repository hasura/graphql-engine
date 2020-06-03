module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict.Extended          as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as S
import qualified Language.GraphQL.Draft.Syntax         as G

import           Control.Lens.Extended
import           Control.Monad.Unique

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser                 (Kind (..), Parser, UnpreparedValue (..), Schema (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Introspect
import           Hasura.GraphQL.Schema.Mutation
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
  -> m (HashMap RoleName GQLContext)
buildGQLContext allTables =
  S.toMap allRoles & Map.traverseWithKey \roleName () ->
    buildContextForRole roleName
  where
    allRoles :: HashSet RoleName
    allRoles = S.insert adminRole $ allTables ^.. folded.tiRolePermInfoMap.to Map.keys.folded

    tableFilter ti = not (isSystemDefined $ _tciSystemDefined ti)

    validTables = Map.filter (tableFilter . _tiCoreInfo) allTables

    buildContextForRole roleName = do
      SQLGenCtx{ stringifyNum } <- askSQLGenCtx
      P.runSchemaT roleName allTables $ GQLContext
        <$> (finalizeParser <$> queryWithIntrospection (S.fromList $ Map.keys validTables) stringifyNum)

    finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
    finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

-- | Generate all the field parsers for query-type GraphQL requests.  We don't
-- actually collect these into a @Parser@ using @selectionSet@ so that we can
-- insert the introspection before doing so.
query'
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m [P.FieldParser n (QueryRootField UnpreparedValue)]
query' allTables stringifyNum = do
  selectExpParsers <- for (toList allTables) \tableName -> do
    selectPerms <- tableSelectPermissions tableName
    for selectPerms \perms -> do
      displayName <- P.qualifiedObjectToName tableName
      let fieldsDesc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName tableName) <> "\""
          aggName = displayName <> $$(G.litName "_aggregate")
          aggDesc = G.Description $ "fetch aggregated fields from the table: \"" <> getTableTxt (qName tableName) <> "\""
          pkName = displayName <> $$(G.litName "_by_pk")
          pkDesc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName tableName) <> "\" using primary key columns"
      catMaybes <$> sequenceA
        [ toQrf QRFSimple      $ selectTable          tableName displayName (Just fieldsDesc) perms stringifyNum
        , toQrf QRFPrimaryKey  $ selectTableByPk      tableName pkName      (Just pkDesc)     perms stringifyNum
        , toQrf QRFAggregation $ selectTableAggregate tableName aggName     (Just aggDesc)    perms stringifyNum
        ]
  pure $ concat $ catMaybes selectExpParsers
  where toQrf = fmap . fmap . fmap

-- | Parse query-type GraphQL requests without introspection (should be unused;
-- just here for completeness)
query
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
query allTables stringifyNum = do
  queryFieldsParser <- query' allTables stringifyNum
  pure $ P.selectionSet $$(G.litName "query_root") Nothing queryFieldsParser
    <&> fmap (P.handleTypename (QRFRaw . J.String . G.unName))

-- | Prepare the parser for query-type GraphQL requests, but with introspection
-- for queries, mutations and subscriptions (TODO) built in.
queryWithIntrospection
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
queryWithIntrospection allTables stringifyNum = do
  fakeQueryFP <- query' allTables stringifyNum
  mutationP <- mutation allTables stringifyNum
  let
    name = $$(G.litName "query_root")
    description = Nothing
    fakeQueryType = P.Nullable $ P.TNamed $ P.mkDefinition name description $
        P.TIObject $ map P.fDefinition fakeQueryFP
    collectTypes :: P.Type 'Output -> m (HashMap G.Name (P.Definition P.SomeTypeInfo))
    collectTypes tp = case P.collectTypeDefinitions tp of
      Left (P.ConflictingDefinitions type1 _) -> throw500 $
        "found conflicting definitions for " <> P.getName type1
        <<> " when collecting types from the schema"
      Right tps -> pure tps
  allQueryTypes <- collectTypes fakeQueryType
  allMutationTypes <- collectTypes (P.pType mutationP)
  let allTypes = Map.union allQueryTypes allMutationTypes
      fakeSchema = Schema
        { sDescription = Nothing
        , sTypes = allTypes
        , sQueryType = fakeQueryType
        , sMutationType = Just $ P.pType mutationP
        -- TODO add subscriptions into introspection when they're done
        , sSubscriptionType = Nothing
        , sDirectives = []
        }
  let realQueryFields =
        fakeQueryFP ++ (fmap QRFRaw <$> [schema fakeSchema, typeIntrospection fakeSchema])
  pure $ P.selectionSet name Nothing realQueryFields
    <&> fmap (P.handleTypename (QRFRaw . J.String . G.unName))

mutation
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)))
mutation allTables stringifyNum = do
  mutationParsers <- for (toList allTables) \table -> do
    displayName <- P.qualifiedObjectToName table
    tablePerms  <- tablePermissions table
    for tablePerms \permissions -> do
      let selectPerms = _permSel permissions

      inserts <- for (_permIns permissions) \insertPerms -> do
        let insertName = $$(G.litName "insert_") <> displayName
            insertDesc = G.Description $ "insert data into the table: \"" <> G.unName displayName <> "\""
            insertOneName = $$(G.litName "insert_") <> displayName <> $$(G.litName "_one")
            insertOneDesc = G.Description $ "insert a single row into the table: \"" <> G.unName displayName <> "\""
        insert <- insertIntoTable table insertName (Just insertDesc) insertPerms selectPerms (_permUpd permissions) stringifyNum
        -- select permissions are required for InsertOne: the
        -- selection set is the same as a select on that table, and it
        -- therefore can't be populated if the user doesn't have
        -- select permissions
        insertOne <- for selectPerms \selPerms ->
          insertOneIntoTable table insertOneName (Just insertOneDesc) insertPerms selPerms (_permUpd permissions) stringifyNum
        pure $ fmap MRFInsert insert : maybe [] (pure . fmap MRFInsert) insertOne

      updates <- for (_permUpd permissions) \updatePerms -> do
        let updateName = $$(G.litName "update_") <> displayName
            updateDesc = G.Description $ "update data of the table: \"" <> G.unName displayName <> "\""
            updateByPkName = $$(G.litName "update_") <> displayName <> $$(G.litName "_by_pk")
            updateByPkDesc = G.Description $ "update single row of the table: \"" <> G.unName displayName <> "\""
        update <- updateTable table updateName (Just updateDesc) updatePerms selectPerms stringifyNum
        -- likewise; furthermore, primary keys can only be tested in
        -- the `where` clause if the user has select permissions for
        -- them, which at the very least requires select permissions
        updateByPk <- join <$> for selectPerms \selPerms ->
          updateTableByPk table updateByPkName (Just updateByPkDesc) updatePerms selPerms stringifyNum
        pure $ fmap MRFUpdate <$> catMaybes [update, updateByPk]

      deletes <- for (_permDel permissions) \deletePerms -> do
        let deleteName = $$(G.litName "delete_") <> displayName
            deleteDesc = G.Description $ "delete data from the table: \"" <> G.unName displayName <> "\""
            deleteByPkName = $$(G.litName "delete_") <> displayName <> $$(G.litName "_by_pk")
            deleteByPkDesc = G.Description $ "delete single row from the table: \"" <> G.unName displayName <> "\""
        delete <- deleteFromTable table deleteName (Just deleteDesc) deletePerms selectPerms stringifyNum
        -- ditto
        deleteByPk <- join <$> for selectPerms \selPerms ->
          deleteFromTableByPk table deleteByPkName (Just deleteByPkDesc) deletePerms selPerms stringifyNum
        pure $ fmap MRFDelete delete : maybe [] (pure . fmap MRFDelete) deleteByPk

      pure $ concat $ catMaybes [inserts, updates, deletes]
  let mutationFieldsParser = concat $ catMaybes mutationParsers
  pure $ P.selectionSet $$(G.litName "mutation_root") Nothing mutationFieldsParser
    <&> fmap (P.handleTypename (MRFRaw . J.String . G.unName))
