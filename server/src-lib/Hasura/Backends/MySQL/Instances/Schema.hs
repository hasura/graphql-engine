{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hasura.Backends.MySQL.Instances.Schema where

import qualified Data.Aeson                                  as J
import qualified Data.HashMap.Strict                         as HM
import qualified Data.List.NonEmpty                          as NE
import           Data.Text.Extended
import qualified Database.MySQL.Base.Types                   as MySQL
import qualified Hasura.Backends.MySQL.Types                 as MySQL
import           Hasura.Base.Error
import           Hasura.GraphQL.Parser                       hiding (EnumValueInfo, field)
import qualified Hasura.GraphQL.Parser                       as P
import           Hasura.GraphQL.Parser.Internal.Parser       hiding (field)
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Schema.Backend
import qualified Hasura.GraphQL.Schema.Build                 as GSB
import           Hasura.GraphQL.Schema.Select
import           Hasura.Prelude
import           Hasura.RQL.IR
import qualified Hasura.RQL.IR.Select                        as IR
import qualified Hasura.RQL.IR.Update                        as IR
import           Hasura.RQL.Types                            as RQL
import qualified Language.GraphQL.Draft.Syntax               as G

instance BackendSchema 'MySQL where
  buildTableQueryFields          = GSB.buildTableQueryFields
  buildTableRelayQueryFields     = buildTableRelayQueryFields'
  buildTableInsertMutationFields = buildTableInsertMutationFields'
  buildTableUpdateMutationFields = buildTableUpdateMutationFields'
  buildTableDeleteMutationFields = buildTableDeleteMutationFields'
  buildFunctionQueryFields       = buildFunctionQueryFields'
  buildFunctionRelayQueryFields  = buildFunctionRelayQueryFields'
  buildFunctionMutationFields    = buildFunctionMutationFields'
  relayExtension                 = Nothing
  tableArguments                 = mysqlTableArgs
  nodesAggExtension              = Just ()
  columnParser                   = columnParser'
  jsonPathArg                    = jsonPathArg'
  orderByOperators               = orderByOperators'
  comparisonExps                 = comparisonExps'
  updateOperators                = updateOperators'
  mkCountType                    = error "MySQL backend does not support this operation yet."
  aggregateOrderByCountType      = error "MySQL backend does not support this operation yet."
  computedField                  = error "MySQL backend does not support this operation yet."
  node                           = error "MySQL backend does not support this operation yet."
  columnDefaultValue             = error "MySQL backend does not support this operation yet."

mysqlTableArgs
  :: forall r m n
   . MonadBuildSchema 'MySQL r m n
  => SourceName
  -> TableInfo 'MySQL
  -> SelPermInfo 'MySQL
  -> m (InputFieldsParser n (IR.SelectArgsG 'MySQL (UnpreparedValue 'MySQL)))
mysqlTableArgs sourceName tableInfo selectPermissions = do
  whereParser   <- tableWhereArg   sourceName tableInfo selectPermissions
  orderByParser <- tableOrderByArg sourceName tableInfo selectPermissions
  pure do
    whereArg   <- whereParser
    orderByArg <- orderByParser
    limitArg   <- tableLimitArg
    offsetArg  <- tableOffsetArg
    pure $ IR.SelectArgs
      { IR._saWhere    = whereArg
      , IR._saOrderBy  = orderByArg
      , IR._saLimit    = limitArg
      , IR._saOffset   = offsetArg
      , IR._saDistinct = Nothing
      }

buildTableRelayQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  NESeq (ColumnInfo 'MySQL) ->
  SelPermInfo 'MySQL ->
  m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
buildTableRelayQueryFields' _sourceName _sourceInfo _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []


buildTableInsertMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  InsPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  Maybe (UpdPermInfo 'MySQL) ->
  m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
buildTableInsertMutationFields' _sourceName _sourceInfo _tableName _tableInfo _gqlName _insPerms _selPerms _updPerms =
  pure []


buildTableUpdateMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  UpdPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
buildTableUpdateMutationFields' _sourceName _sourceInfo _tableName _tableInfo _gqlName _updPerns _selPerms =
  pure []


buildTableDeleteMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  RQL.TableName 'MySQL ->
  TableInfo 'MySQL ->
  G.Name ->
  DelPermInfo 'MySQL ->
  Maybe (SelPermInfo 'MySQL) ->
  m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
buildTableDeleteMutationFields' _sourceName _sourceInfo _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []


buildFunctionQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  SelPermInfo 'MySQL ->
  m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
buildFunctionQueryFields' _ _ _ _ _ _ =
  pure []


buildFunctionRelayQueryFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  NESeq (ColumnInfo 'MySQL) ->
  SelPermInfo 'MySQL ->
  m [(FieldParser n (QueryRootField UnpreparedValue UnpreparedValue))]
buildFunctionRelayQueryFields' _sourceName _sourceInfo _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure []


buildFunctionMutationFields' ::
  MonadBuildSchema 'MySQL r m n =>
  SourceName ->
  RQL.SourceConfig 'MySQL ->
  FunctionName 'MySQL ->
  FunctionInfo 'MySQL ->
  RQL.TableName 'MySQL ->
  SelPermInfo 'MySQL ->
  m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
buildFunctionMutationFields' _ _ _ _ _ _ =
  pure []


columnParser' :: (MonadSchema n m, MonadError QErr m) =>
  ColumnType 'MySQL ->
  G.Nullability ->
  m (Parser 'Both n (Opaque (ColumnValue 'MySQL)))
columnParser' columnType (G.Nullability isNullable) =
  opaque . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> case scalarType of
      MySQL.Bit      -> pure $ possiblyNullable scalarType $ MySQL.BitValue <$> P.boolean
      MySQL.String   -> pure $ possiblyNullable scalarType $ MySQL.VarcharValue <$> P.string
      MySQL.Decimal  -> pure $ possiblyNullable scalarType $ MySQL.DecimalValue <$> P.float
      MySQL.Double   -> pure $ possiblyNullable scalarType $ MySQL.DoubleValue <$> P.float
      MySQL.Float    -> pure $ possiblyNullable scalarType $ MySQL.FloatValue <$> P.float
      MySQL.Int24    -> pure $ possiblyNullable scalarType $ MySQL.MediumValue <$> P.int
      MySQL.LongLong -> pure $ possiblyNullable scalarType $ MySQL.BigValue <$> P.int
      MySQL.Long     -> pure $ possiblyNullable scalarType $ MySQL.IntValue <$> P.int
      MySQL.Short    -> pure $ possiblyNullable scalarType $ MySQL.SmallValue <$> P.int
      MySQL.Tiny     -> pure $ possiblyNullable scalarType $ MySQL.TinyValue <$> P.int
      _              -> pure $ possiblyNullable scalarType $ MySQL.NullValue <$ P.string -- TODO: Complete this
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (HM.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName @'MySQL tableName `onLeft` throwError
          let enumName = tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable MySQL.VarChar $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    opaque :: MonadParse m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          P.GraphQLValue (G.VVariable var@Variable{ vInfo, vValue }) -> do
            typeCheck False (P.toGraphQLType $ pType parser) var
            P.mkOpaque (Just vInfo) <$> pParser parser (absurd <$> vValue)
          value -> P.mkOpaque Nothing <$> pParser parser value
      }
    possiblyNullable :: (MonadParse m) => MySQL.Type -> Parser 'Both m MySQL.ScalarValue -> Parser 'Both m MySQL.ScalarValue
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe MySQL.NullValue) . P.nullable
      | otherwise  = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, RQL.ScalarValue 'MySQL)
    mkEnumValue (RQL.EnumValue value, EnumValueInfo description) =
      ( P.mkDefinition value (G.Description <$> description) P.EnumValueInfo
      , MySQL.VarcharValue $ G.unName value
      )
    throughJSON scalarName =
      let schemaType = P.NonNullable $ P.TNamed $ P.mkDefinition scalarName Nothing P.TIScalar
      in  Parser
          { pType = schemaType
          , pParser =
            valueToJSON (P.toGraphQLType schemaType) >=>
            either (parseErrorWith ParseFailed . qeError) pure . runAesonParser J.parseJSON
          }


jsonPathArg' ::
  MonadParse n =>
  ColumnType 'MySQL ->
  InputFieldsParser n (Maybe (IR.ColumnOp 'MySQL))
jsonPathArg' _columnType = pure Nothing


orderByOperators' :: NonEmpty (Definition P.EnumValueInfo, (BasicOrderType 'MySQL, NullsOrderType 'MySQL))
orderByOperators' =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls first"
      , (MySQL.Asc, MySQL.NullsFirst)
      )
    , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
      , (MySQL.Asc, MySQL.NullsFirst)
      )
    , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
      , (MySQL.Asc, MySQL.NullsLast)
      )
    , ( define $$(G.litName "desc") "in descending order, nulls last"
      , (MySQL.Desc, MySQL.NullsLast)
      )
    , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
      , (MySQL.Desc, MySQL.NullsFirst)
      )
    , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
      , (MySQL.Desc, MySQL.NullsLast)
      )
    ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo


-- | TODO: Make this as thorough as the one for MSSQL/PostgreSQL
comparisonExps' ::
  forall m n. (BackendSchema 'MySQL, MonadSchema n m, MonadError QErr m) =>
  ColumnType 'MySQL ->
  m (Parser 'Input n [ComparisonExp 'MySQL])
comparisonExps' = P.memoize 'comparisonExps $ \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  typedParser        <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar @'MySQL MySQL.VarChar) (G.Nullability True)
  textParser         <- columnParser (ColumnScalar @'MySQL MySQL.VarChar) (G.Nullability False)
  let name = P.getName typedParser <> $$(G.litName "_MySQL_comparison_exp")
      desc = G.Description $ "Boolean expression to compare columns of type "
        <>  P.getName typedParser
        <<> ". All fields are combined with logical 'AND'."
      textListParser = P.list textParser `P.bind` traverse P.openOpaque
      columnListParser = P.list typedParser `P.bind` traverse P.openOpaque
  pure $ P.object name (Just desc) $ catMaybes <$> sequenceA
    [ P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean)
    , P.fieldOptional $$(G.litName "_eq")      Nothing (AEQ True . mkParameter <$> typedParser)
    , P.fieldOptional $$(G.litName "_neq")     Nothing (ANE True . mkParameter <$> typedParser)
    , P.fieldOptional $$(G.litName "_gt")      Nothing (AGT  . mkParameter <$> typedParser)
    , P.fieldOptional $$(G.litName "_lt")      Nothing (ALT  . mkParameter <$> typedParser)
    , P.fieldOptional $$(G.litName "_gte")     Nothing (AGTE . mkParameter <$> typedParser)
    , P.fieldOptional $$(G.litName "_lte")     Nothing (ALTE . mkParameter <$> typedParser)
    ]


offsetParser' :: MonadParse n => Parser 'Both n (SQLExpression 'MySQL)
offsetParser' =
  MySQL.ValueExpression . MySQL.BigValue . fromIntegral <$> P.int

-- | Various update operators
updateOperators' ::
  Applicative m =>
  -- | qualified name of the table
  TableInfo 'MySQL ->
  -- | update permissions of the table
  UpdPermInfo 'MySQL ->
  m (Maybe (InputFieldsParser n [(RQL.Column 'MySQL, IR.UpdOpExpG (UnpreparedValue 'MySQL))]))
updateOperators' _table _updatePermissions = pure Nothing
