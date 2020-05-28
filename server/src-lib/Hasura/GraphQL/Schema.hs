module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict.Extended          as M
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as S
import qualified Language.GraphQL.Draft.Syntax         as G

import           Control.Lens.Extended
import           Control.Monad.Unique

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser                 (Kind (..), Parser, UnpreparedValue (..))
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
  S.toMap allRoles & M.traverseWithKey \roleName () ->
    buildContextForRole roleName
  where
    allRoles :: HashSet RoleName
    allRoles = S.insert adminRole $ allTables ^.. folded.tiRolePermInfoMap.to M.keys.folded

    buildContextForRole roleName = do
      SQLGenCtx{ stringifyNum } <- askSQLGenCtx
      P.runSchemaT roleName allTables $ GQLContext
        <$> (finalizeParser <$> query (S.fromList $ M.keys allTables) stringifyNum)

    finalizeParser :: Parser 'Output (P.ParseT Identity) a -> ParserFn a
    finalizeParser parser = runIdentity . P.runParseT . P.runParser parser

query
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (OMap.InsOrdHashMap G.Name (QueryRootField UnpreparedValue)))
query allTables stringifyNum = do
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
  let queryFieldsParser = concat $ catMaybes selectExpParsers
      queryType = P.NonNullable $ P.TNamed $ P.mkDefinition $$(G.litName "query_root") Nothing $
                  P.TIObject $ map P.fDefinition queryFieldsParser
      realSchema = Schema
        { sDescription = Nothing
        , sTypes = [queryType]
        , sQueryType = queryType
        , sMutationType = Nothing
        , sSubscriptionType = Nothing
        , sDirectives = []
        }
      schemaFieldParser = schema realSchema
  let queryFieldsParserWithIntrospection = queryFieldsParser ++ [fmap QRFRaw schemaFieldParser]
  pure $ P.selectionSet $$(G.litName "query_root") Nothing queryFieldsParserWithIntrospection
    <&> fmap (P.handleTypename (QRFRaw . J.String . G.unName))
  where toQrf = fmap . fmap . fmap


mutation
  :: forall m n. (MonadSchema n m, MonadError QErr m)
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
