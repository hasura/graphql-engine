module Hasura.GraphQL.Schema where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Parser         (Kind (..), Parser, UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types


-- FIXME: taken from Resolve.hs
-- do we want to keep it the same?
data QueryRootField
  = QRFSimple      !(RQL.AnnSimpleSelG UnpreparedValue)
  | QRFPrimaryKey  !(RQL.AnnSimpleSelG UnpreparedValue)
  | QRFAggregation !(RQL.AnnAggSelG    UnpreparedValue)

query
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (HashMap G.Name QueryRootField))
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
        [ toQrf QRFSimple      $ Just <$> selectTable tableName displayName (Just fieldsDesc) perms stringifyNum
        , toQrf QRFPrimaryKey  $ selectTableByPk      tableName pkName      (Just pkDesc)     perms stringifyNum
        , toQrf QRFAggregation $ selectTableAggregate tableName aggName     (Just aggDesc)    perms stringifyNum
        ]
  -- handle __typename
  let queryFieldsParser = fmap (M.fromList . catMaybes) $ sequenceA $ concat $ catMaybes selectExpParsers
  pure $ P.selectionSet $$(G.litName "Query") Nothing queryFieldsParser
  where toQrf = fmap . fmap . fmap . fmap . fmap
