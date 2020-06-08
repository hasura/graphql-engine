module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp
  ) where

import           Hasura.Prelude

import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Parser         (InputFieldsParser, Kind (..), Parser,
                                                UnpreparedValue)
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
  tableFields  <- tableSelectFields table selectPermissions
  fieldParsers <- sequenceA . catMaybes <$> traverse mkField tableFields
  pure $ fmap (concat . catMaybes) $ P.object name (Just description) fieldParsers
  where
    mkField
      :: FieldInfo
      -> m (Maybe (InputFieldsParser n (Maybe [RQL.AnnOrderByItemG UnpreparedValue])))
    mkField fieldInfo = runMaybeT $ do
      case fieldInfo of
        FIColumn columnInfo -> do
          let fieldName = pgiName columnInfo
          pure $ P.fieldOptional fieldName Nothing orderByOperator
            `mapField` (pure . mkOrderByItemG (RQL.AOCPG $ pgiColumn columnInfo))
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          fieldName <- MaybeT $ pure $ G.mkName $ relNameToTxt $ riName relationshipInfo
          perms <- MaybeT $ tableSelectPermissions remoteTable
          let newPerms = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter perms
          case riType relationshipInfo of
            ObjRel -> do
              otherTableParser <- lift $ orderByExp remoteTable perms
              pure $ P.fieldOptional fieldName Nothing otherTableParser `mapField`
                map (fmap $ RQL.AOCObj relationshipInfo newPerms)
            ArrRel -> do
              let aggregateFieldName = fieldName <> $$(G.litName "_aggregate")
              aggregationParser <- lift $ orderByAggregation remoteTable perms
              pure $ P.fieldOptional aggregateFieldName Nothing aggregationParser `mapField`
                map (fmap $ RQL.AOCAgg relationshipInfo newPerms)
        FIComputedField _ -> empty



-- local definitions

type OrderInfo = (SQL.OrderType, SQL.NullsOrder)


orderByAggregation
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Parser 'Input n [OrderByItemG RQL.AnnAggOrdBy])
orderByAggregation table selectPermissions = do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableName  <- qualifiedObjectToName table
  allColumns <- tableSelectColumns table selectPermissions
  let numColumns  = onlyNumCols allColumns
      compColumns = onlyComparableCols allColumns
      numFields   = catMaybes <$> traverse mkField numColumns
      compFields  = catMaybes <$> traverse mkField compColumns
      aggFields   = fmap (concat . catMaybes . concat) $ sequenceA $ catMaybes
        [ -- count
          Just $ P.fieldOptional $$(G.litName "count") Nothing orderByOperator
            <&> pure . fmap (pure . mkOrderByItemG RQL.AAOCount)
        , -- operators on numeric columns
          if null numColumns then Nothing else Just $
          for numericAggOperators \operator ->
            parseOperator operator tableName numFields
        , -- operators on comparable columns
          if null compColumns then Nothing else Just $
          for comparisonAggOperators \operator ->
            parseOperator operator tableName compFields
        ]
  let objectName  = tableName <> $$(G.litName "_aggregate_order_by")
      description = G.Description $ "order by aggregate values of table \"" <> table <<> "\""
  pure $ P.object objectName (Just description) aggFields
  where
    mkField :: PGColumnInfo -> InputFieldsParser n (Maybe (PGCol, OrderInfo))
    mkField columnInfo =
      P.fieldOptional (pgiName columnInfo) (pgiDescription columnInfo) orderByOperator
        `mapField` (pgiColumn columnInfo,)

    parseOperator
      :: G.Name
      -> G.Name
      -> InputFieldsParser n [(PGCol, OrderInfo)]
      -> InputFieldsParser n (Maybe [OrderByItemG RQL.AnnAggOrdBy])
    parseOperator operator tableName columns =
      let opText     = G.unName operator
          -- FIXME: isn't G.Name a Monoid?
          objectName = foldr1 (<>) [ tableName
                                   , $$(G.litName "_")
                                   , operator
                                   , $$(G.litName "_order_by")
                                   ]
          objectDesc = Just $ G.Description $ mconcat [ "order by"
                                                      , opText
                                                      , "() on columns of table "
                                                      , G.unName tableName
                                                      , "\""
                                                      ]
      in  P.fieldOptional operator Nothing (P.object objectName objectDesc columns)
        `mapField` map (\(col, info) -> mkOrderByItemG (RQL.AAOOp opText col) info)



orderByOperator :: MonadParse m => Parser 'Both m OrderInfo
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



-- local helpers

mkOrderByItemG :: a -> OrderInfo -> OrderByItemG a
mkOrderByItemG column (orderType, nullsOrder) =
  OrderByItemG { obiType   = Just $ RQL.OrderType orderType
               , obiColumn = column
               , obiNulls  = Just $ RQL.NullsOrder nullsOrder
               }

aliasToName :: G.Name -> FieldName
aliasToName = FieldName . G.unName
