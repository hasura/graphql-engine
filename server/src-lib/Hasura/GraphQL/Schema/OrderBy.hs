module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp
  ) where

import           Hasura.Prelude

import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Parser         (Kind (..), Parser, UnpreparedValue)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types              as RQL
import           Hasura.SQL.DML                as SQL
import           Hasura.SQL.Types

-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
orderByExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Parser 'Input n [RQL.AnnOrderByItemG UnpreparedValue])
orderByExp table selectPermissions = memoizeOn 'orderByExp table $ do
  name <- qualifiedObjectToName table <&> (<> $$(G.litName "_order_by"))
  let description = G.Description $
        "Ordering options when selecting data from " <> table <<> "."
  tableFieldParsers <- fmap catMaybes $ traverse mkField =<< tableSelectFields table selectPermissions
  pure $ fmap (concat . catMaybes) $ P.object name (Just description) $ sequenceA tableFieldParsers
  where
    mkField fieldInfo = join <$> for (fieldInfoGraphQLName fieldInfo) \fieldName ->
      case fieldInfo of
        FIColumn columnInfo ->
          pure $ Just $ P.fieldOptional fieldName Nothing orderByOperator <&> fmap
            \(orderType, nullsOrder) -> pure $ OrderByItemG
              { obiType   = Just $ RQL.OrderType orderType
              , obiColumn = RQL.AOCPG $ pgiColumn columnInfo
              , obiNulls  = Just $ RQL.NullsOrder nullsOrder
              }
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          remotePermissions <- tableSelectPermissions remoteTable
          for remotePermissions $ \perms -> do
            otherTableParser <- orderByExp remoteTable perms
            case riType relationshipInfo of
              ObjRel -> pure $ P.fieldOptional fieldName Nothing otherTableParser <&>
                fmap (map \orderByItem ->
                         orderByItem { obiColumn = RQL.AOCObj
                                       relationshipInfo
                                       (fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter perms)
                                       (obiColumn orderByItem)
                                     }
                     )
              ArrRel -> undefined -- FIXME
        FIComputedField _ -> pure Nothing

orderByOperator :: MonadParse m => Parser 'Both m (SQL.OrderType, SQL.NullsOrder)
orderByOperator =
  P.enum $$(G.litName "order_by") (Just "column ordering options") $ NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls last"
      , (SQL.OTAsc, SQL.NLast)
      )
    , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
      , (SQL.OTAsc, SQL.NFirst)
      )
    , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
      , (SQL.OTAsc, SQL.NLast)
      )
    , ( define $$(G.litName "desc") "in descending order, nulls first"
      , (SQL.OTAsc, SQL.NFirst)
      )
    , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
      , (SQL.OTAsc, SQL.NFirst)
      )
    , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
      , (SQL.OTAsc, SQL.NLast)
      )
    ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo
