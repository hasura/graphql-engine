{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Schema () where

import Data.ByteString (ByteString)
import Data.Has
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended
import Database.MySQL.Base.Types qualified as MySQL
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    MonadMemoize,
    MonadParse,
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Typename (MkTypename)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Backend as RQL
import Hasura.RQL.Types.Column as RQL
import Hasura.RQL.Types.SchemaCache as RQL
import Hasura.RQL.Types.Source as RQL
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as GQL

instance BackendSchema 'MySQL where
  buildTableQueryAndSubscriptionFields = GSB.buildTableQueryAndSubscriptionFields
  buildTableRelayQueryFields _ _ _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields _ _ _ _ _ _ = pure []
  buildTableUpdateMutationFields _ _ _ _ _ _ = pure []
  buildTableDeleteMutationFields _ _ _ _ _ _ = pure []
  buildFunctionQueryFields _ _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ _ = pure []
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing
  columnParser = columnParser'
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators _sourceInfo = orderByOperators'
  comparisonExps = comparisonExps'
  countTypeInput = mysqlCountTypeInput
  aggregateOrderByCountType = error "aggregateOrderByCountType: MySQL backend does not support this operation yet."
  computedField = error "computedField: MySQL backend does not support this operation yet."

instance BackendTableSelectSchema 'MySQL where
  tableArguments = mysqlTableArgs
  selectTable = defaultSelectTable
  selectTableAggregate = defaultSelectTableAggregate
  tableSelectionSet = defaultTableSelectionSet

mysqlTableArgs ::
  forall r m n.
  MonadBuildSchema 'MySQL r m n =>
  RQL.SourceInfo 'MySQL ->
  TableInfo 'MySQL ->
  m (InputFieldsParser n (IR.SelectArgsG 'MySQL (UnpreparedValue 'MySQL)))
mysqlTableArgs sourceInfo tableInfo = do
  whereParser <- tableWhereArg sourceInfo tableInfo
  orderByParser <- tableOrderByArg sourceInfo tableInfo
  pure do
    whereArg <- whereParser
    orderByArg <- orderByParser
    limitArg <- tableLimitArg
    offsetArg <- tableOffsetArg
    pure $
      IR.SelectArgs
        { IR._saWhere = whereArg,
          IR._saOrderBy = orderByArg,
          IR._saLimit = limitArg,
          IR._saOffset = offsetArg,
          IR._saDistinct = Nothing
        }

bsParser :: MonadParse m => Parser 'Both m ByteString
bsParser = encodeUtf8 <$> P.string

columnParser' ::
  (MonadParse n, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  ColumnType 'MySQL ->
  GQL.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (ColumnValue 'MySQL)))
columnParser' columnType (GQL.Nullability isNullable) =
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> case scalarType of
      MySQL.Decimal -> pure $ possiblyNullable scalarType $ MySQL.DecimalValue <$> P.float
      MySQL.Tiny -> pure $ possiblyNullable scalarType $ MySQL.TinyValue <$> P.int
      MySQL.Short -> pure $ possiblyNullable scalarType $ MySQL.SmallValue <$> P.int
      MySQL.Long -> pure $ possiblyNullable scalarType $ MySQL.IntValue <$> P.int
      MySQL.Float -> pure $ possiblyNullable scalarType $ MySQL.FloatValue <$> P.float
      MySQL.Double -> pure $ possiblyNullable scalarType $ MySQL.DoubleValue <$> P.float
      MySQL.Null -> pure $ possiblyNullable scalarType $ MySQL.NullValue <$ P.string
      MySQL.LongLong -> pure $ possiblyNullable scalarType $ MySQL.BigValue <$> P.int
      MySQL.Int24 -> pure $ possiblyNullable scalarType $ MySQL.MediumValue <$> P.int
      MySQL.Date -> pure $ possiblyNullable scalarType $ MySQL.DateValue <$> P.string
      MySQL.Year -> pure $ possiblyNullable scalarType $ MySQL.YearValue <$> P.string
      MySQL.Bit -> pure $ possiblyNullable scalarType $ MySQL.BitValue <$> P.boolean
      MySQL.String -> pure $ possiblyNullable scalarType $ MySQL.VarcharValue <$> P.string
      MySQL.VarChar -> pure $ possiblyNullable scalarType $ MySQL.VarcharValue <$> P.string
      MySQL.DateTime -> pure $ possiblyNullable scalarType $ MySQL.DatetimeValue <$> P.string
      MySQL.Blob -> pure $ possiblyNullable scalarType $ MySQL.BlobValue <$> bsParser
      MySQL.Timestamp -> pure $ possiblyNullable scalarType $ MySQL.TimestampValue <$> P.string
      _ -> do
        name <- MySQL.mkMySQLScalarTypeName scalarType
        let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing Nothing [] P.TIScalar
        pure $
          P.Parser
            { pType = schemaType,
              pParser =
                P.valueToJSON (P.toGraphQLType schemaType)
                  >=> either (P.parseErrorWith P.ParseFailed . toErrorMessage . qeError) pure . (MySQL.parseScalarValue scalarType)
            }
    ColumnEnumReference enumRef@(EnumReference _ enumValues _) ->
      case nonEmpty (HM.toList enumValues) of
        Just enumValuesList -> do
          enumName <- mkEnumTypeName enumRef
          pure $ possiblyNullable MySQL.VarChar $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable :: (MonadParse m) => MySQL.Type -> Parser 'Both m MySQL.ScalarValue -> Parser 'Both m MySQL.ScalarValue
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe MySQL.NullValue) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, RQL.ScalarValue 'MySQL)
    mkEnumValue (RQL.EnumValue value, EnumValueInfo description) =
      ( P.Definition value (GQL.Description <$> description) Nothing [] P.EnumValueInfo,
        MySQL.VarcharValue $ GQL.unName value
      )

orderByOperators' :: NamingCase -> (GQL.Name, NonEmpty (P.Definition P.EnumValueInfo, (BasicOrderType 'MySQL, NullsOrderType 'MySQL)))
orderByOperators' _tCase =
  (Name._order_by,) $
    -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
    NE.fromList
      [ ( define Name._asc "in ascending order, nulls first",
          (MySQL.Asc, MySQL.NullsFirst)
        ),
        ( define Name._asc_nulls_first "in ascending order, nulls first",
          (MySQL.Asc, MySQL.NullsFirst)
        ),
        ( define Name._asc_nulls_last "in ascending order, nulls last",
          (MySQL.Asc, MySQL.NullsLast)
        ),
        ( define Name._desc "in descending order, nulls last",
          (MySQL.Desc, MySQL.NullsLast)
        ),
        ( define Name._desc_nulls_first "in descending order, nulls first",
          (MySQL.Desc, MySQL.NullsFirst)
        ),
        ( define Name._desc_nulls_last "in descending order, nulls last",
          (MySQL.Desc, MySQL.NullsLast)
        )
      ]
  where
    define name desc = P.Definition name (Just desc) Nothing [] P.EnumValueInfo

-- | TODO: Make this as thorough as the one for MSSQL/PostgreSQL
comparisonExps' ::
  forall m n r.
  (BackendSchema 'MySQL, MonadMemoize m, MonadParse n, MonadError QErr m, MonadReader r m, Has MkTypename r, Has NamingCase r) =>
  ColumnType 'MySQL ->
  m (Parser 'Input n [ComparisonExp 'MySQL])
comparisonExps' = P.memoize 'comparisonExps $ \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  typedParser <- columnParser columnType (GQL.Nullability False)
  let name = P.getName typedParser <> Name.__MySQL_comparison_exp
      desc =
        GQL.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
  pure $
    P.object name (Just desc) $
      catMaybes
        <$> sequenceA
          [ P.fieldOptional Name.__is_null Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean),
            P.fieldOptional Name.__eq Nothing (AEQ True . mkParameter <$> typedParser),
            P.fieldOptional Name.__neq Nothing (ANE True . mkParameter <$> typedParser),
            P.fieldOptional Name.__gt Nothing (AGT . mkParameter <$> typedParser),
            P.fieldOptional Name.__lt Nothing (ALT . mkParameter <$> typedParser),
            P.fieldOptional Name.__gte Nothing (AGTE . mkParameter <$> typedParser),
            P.fieldOptional Name.__lte Nothing (ALTE . mkParameter <$> typedParser)
          ]

mysqlCountTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'MySQL)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'MySQL)
mysqlCountTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional Name._columns Nothing $ P.list columnEnum
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [Column 'MySQL] -> CountType 'MySQL
    mkCountType _ Nothing = MySQL.StarCountable
    mkCountType IR.SelectCountDistinct (Just cols) =
      maybe MySQL.StarCountable MySQL.DistinctCountable $ nonEmpty cols
    mkCountType IR.SelectCountNonDistinct (Just cols) =
      maybe MySQL.StarCountable MySQL.NonNullFieldCountable $ nonEmpty cols
