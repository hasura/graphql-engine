module Hasura.GraphQL.Schema
  ( buildGQLContext
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict.Extended  as M
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.HashSet                  as S
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens.Extended
import           Control.Monad.Unique

import qualified Hasura.GraphQL.Parser         as P
-- import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser         (Kind (..), Parser, UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
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
  pure $ P.selectionSet $$(G.litName "query_root") Nothing queryFieldsParser
    <&> fmap (P.handleTypename (QRFRaw . J.String . G.unName))
  where toQrf = fmap . fmap . fmap
