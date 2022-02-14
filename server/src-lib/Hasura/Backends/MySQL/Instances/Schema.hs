{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Schema () where

import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.Has
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended
import Database.MySQL.Base.Types qualified as MySQL
import Hasura.Backends.MySQL.Types qualified as MySQL
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Select
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types as RQL
import Language.GraphQL.Draft.Syntax qualified as G

instance BackendSchema 'MySQL where
  buildTableQueryFields = GSB.buildTableQueryFields
  buildTableRelayQueryFields = buildTableRelayQueryFields'
  buildTableInsertMutationFields = buildTableInsertMutationFields'
  buildTableUpdateMutationFields = buildTableUpdateMutationFields'
  buildTableDeleteMutationFields = buildTableDeleteMutationFields'
  buildFunctionQueryFields = buildFunctionQueryFields'
  buildFunctionRelayQueryFields = buildFunctionRelayQueryFields'
  buildFunctionMutationFields = buildFunctionMutationFields'
  relayExtension = Nothing
  tableArguments = mysqlTableArgs
  nodesAggExtension = Just ()
  columnParser = columnParser'
  jsonPathArg = jsonPathArg'
  orderByOperators = orderByOperators'
  comparisonExps = comparisonExps'
  countTypeInput = mysqlCountTypeInput
  aggregateOrderByCountType = error "aggregateOrderByCountType: MySQL backend does not support this operation yet."
  computedField = error "computedField: MySQL backend does not support this operation yet."
  node = error "node: MySQL backend does not support this operation yet."
  columnDefaultValue = error "columnDefaultValue: MySQL backend does not support this operation yet."

mysqlTableArgs ::
  forall r m n.
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  TableInfo 'MySQL ->
  SelPermInfo 'MySQL ->
  m (InputFieldsParser n (IR.SelectArgsG 'MySQL (UnpreparedValue 'MySQL)))
mysqlTableArgs sourceName tableInfo selectPermissions = do
  whereParser <- tableWhereArg sourceName tableInfo selectPermissions
  orderByParser <- tableOrderByArg sourceName tableInfo selectPermissions
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

buildTableRelayQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  NESeq (ColumnInfo 'MySQL) ->
  SelPermInfo 'MySQL ->
  m [a]
buildTableRelayQueryFields' _sourceName _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []

buildTableInsertMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  InsPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  Maybe (UpdPermInfo 'MySQL) ->
  m [a]
buildTableInsertMutationFields' _sourceName _tableName _tableInfo _gqlName _insPerms _selPerms _updPerms =
  pure []

buildTableUpdateMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  UpdPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  m [a]
buildTableUpdateMutationFields' _sourceName _tableName _tableInfo _gqlName _updPerns _selPerms =
  pure []

buildTableDeleteMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  DelPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  m [a]
buildTableDeleteMutationFields' _sourceName _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []

buildFunctionQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  SelPermInfo 'MySQL ->
  m [a]
buildFunctionQueryFields' _ _ _ _ _ =
  pure []

buildFunctionRelayQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  NESeq (ColumnInfo 'MySQL) ->
  SelPermInfo 'MySQL ->
  m [a]
buildFunctionRelayQueryFields' _sourceName _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure []

buildFunctionMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  SelPermInfo 'MySQL ->
  m [a]
buildFunctionMutationFields' _ _ _ _ _ =
  pure []

bsParser :: MonadParse m => Parser 'Both m ByteString
bsParser = encodeUtf8 <$> P.string

columnParser' ::
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  ColumnType 'MySQL ->
  G.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (ColumnValue 'MySQL)))
columnParser' columnType (G.Nullability isNullable) =
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
        let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing P.TIScalar
        pure $
          Parser
            { pType = schemaType,
              pParser =
                valueToJSON (P.toGraphQLType schemaType)
                  >=> either (parseErrorWith ParseFailed . qeError) pure . (MySQL.parseScalarValue scalarType)
            }
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (HM.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName @'MySQL tableName `onLeft` throwError
          enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable MySQL.VarChar $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable :: (MonadParse m) => MySQL.Type -> Parser 'Both m MySQL.ScalarValue -> Parser 'Both m MySQL.ScalarValue
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe MySQL.NullValue) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, RQL.ScalarValue 'MySQL)
    mkEnumValue (RQL.EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) P.EnumValueInfo,
        MySQL.VarcharValue $ G.unName value
      )
    throughJSON scalarName =
      let schemaType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing P.TIScalar
       in Parser
            { pType = schemaType,
              pParser =
                valueToJSON (P.toGraphQLType schemaType)
                  >=> either (parseErrorWith ParseFailed . qeError) pure . runAesonParser J.parseJSON
            }

jsonPathArg' ::
  MonadParse n =>
  ColumnType 'MySQL ->
  InputFieldsParser n (Maybe (IR.ColumnOp 'MySQL))
jsonPathArg' _columnType = pure Nothing

orderByOperators' :: NonEmpty (Definition P.EnumValueInfo, (BasicOrderType 'MySQL, NullsOrderType 'MySQL))
orderByOperators' =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls first",
        (MySQL.Asc, MySQL.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first",
        (MySQL.Asc, MySQL.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last",
        (MySQL.Asc, MySQL.NullsLast)
      ),
      ( define $$(G.litName "desc") "in descending order, nulls last",
        (MySQL.Desc, MySQL.NullsLast)
      ),
      ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first",
        (MySQL.Desc, MySQL.NullsFirst)
      ),
      ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last",
        (MySQL.Desc, MySQL.NullsLast)
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

-- | TODO: Make this as thorough as the one for MSSQL/PostgreSQL
comparisonExps' ::
  forall m n r.
  (BackendSchema 'MySQL, MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  ColumnType 'MySQL ->
  m (Parser 'Input n [ComparisonExp 'MySQL])
comparisonExps' = P.memoize 'comparisonExps $ \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  typedParser <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar @'MySQL MySQL.VarChar) (G.Nullability True)
  textParser <- columnParser (ColumnScalar @'MySQL MySQL.VarChar) (G.Nullability False)
  let name = P.getName typedParser <> $$(G.litName "_MySQL_comparison_exp")
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      textListParser = fmap openValueOrigin <$> P.list textParser
      columnListParser = fmap openValueOrigin <$> P.list typedParser
  pure $
    P.object name (Just desc) $
      catMaybes
        <$> sequenceA
          [ P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean),
            P.fieldOptional $$(G.litName "_eq") Nothing (AEQ True . mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_neq") Nothing (ANE True . mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_gt") Nothing (AGT . mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_lt") Nothing (ALT . mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_gte") Nothing (AGTE . mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_lte") Nothing (ALTE . mkParameter <$> typedParser)
          ]

offsetParser' :: MonadParse n => Parser 'Both n (SQLExpression 'MySQL)
offsetParser' =
  MySQL.ValueExpression . MySQL.BigValue . fromIntegral <$> P.int

mysqlCountTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'MySQL)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'MySQL)
mysqlCountTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional $$(G.litName "columns") Nothing $ P.list columnEnum
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [Column 'MySQL] -> CountType 'MySQL
    mkCountType _ Nothing = MySQL.StarCountable
    mkCountType IR.SelectCountDistinct (Just cols) =
      maybe MySQL.StarCountable MySQL.DistinctCountable $ nonEmpty cols
    mkCountType IR.SelectCountNonDistinct (Just cols) =
      maybe MySQL.StarCountable MySQL.NonNullFieldCountable $ nonEmpty cols
