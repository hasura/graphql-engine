{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Schema () where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Text.Extended
import qualified Hasura.Backends.BigQuery.Types as BigQuery
import           Hasura.GraphQL.Context
import qualified Hasura.GraphQL.Parser as P
import           Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import           Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import           Hasura.GraphQL.Schema.Backend
import qualified Hasura.GraphQL.Schema.Build as GSB
import           Hasura.GraphQL.Schema.Common
import           Hasura.Prelude
import qualified Hasura.RQL.IR.Select as IR
import qualified Hasura.RQL.IR.Update as IR
import           Hasura.RQL.Types
import qualified Hasura.RQL.Types.Error as RQL
import qualified Language.GraphQL.Draft.Syntax as G

----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'BigQuery where
  -- top level parsers
  buildTableQueryFields          = GSB.buildTableQueryFields
  buildTableRelayQueryFields     = msBuildTableRelayQueryFields
  buildTableInsertMutationFields = msBuildTableInsertMutationFields
  buildTableUpdateMutationFields = msBuildTableUpdateMutationFields
  buildTableDeleteMutationFields = msBuildTableDeleteMutationFields
  buildFunctionQueryFields       = msBuildFunctionQueryFields
  buildFunctionRelayQueryFields  = msBuildFunctionRelayQueryFields
  buildFunctionMutationFields    = msBuildFunctionMutationFields
  -- backend extensions
  relayExtension    = const Nothing
  nodesAggExtension = const $ Just ()
  -- indivdual components
  columnParser              = msColumnParser
  jsonPathArg               = msJsonPathArg
  orderByOperators          = msOrderByOperators
  comparisonExps            = msComparisonExps
  updateOperators           = msUpdateOperators
  offsetParser              = msOffsetParser
  mkCountType               = msMkCountType
  aggregateOrderByCountType = BigQuery.IntegerScalarType
  computedField             = msComputedField
  node                      = msNode
  tableDistinctOn           = msTableDistinctOn
  remoteRelationshipField   = msRemoteRelationshipField
  -- SQL literals
  columnDefaultValue = error "TODO: Make impossible by the type system. BigQuery doesn't support insertions."

----------------------------------------------------------------
-- Top level parsers

msBuildTableRelayQueryFields
  :: MonadBuildSchema 'BigQuery r m n
  => SourceName
  -> SourceConfig 'BigQuery
  -> TableName    'BigQuery
  -> TableInfo    'BigQuery
  -> G.Name
  -> NESeq (ColumnInfo 'BigQuery)
  -> SelPermInfo  'BigQuery
  -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
msBuildTableRelayQueryFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure Nothing

msBuildTableInsertMutationFields
  :: MonadBuildSchema 'BigQuery r m n
  => SourceName
  -> SourceConfig 'BigQuery
  -> TableName 'BigQuery
  -> TableInfo 'BigQuery
  -> G.Name
  -> InsPermInfo 'BigQuery
  -> Maybe (SelPermInfo 'BigQuery)
  -> Maybe (UpdPermInfo 'BigQuery)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
msBuildTableInsertMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _insPerms _selPerms _updPerms =
  pure []

msBuildTableUpdateMutationFields
  :: MonadBuildSchema 'BigQuery r m n
  => SourceName
  -> SourceConfig 'BigQuery
  -> TableName 'BigQuery
  -> TableInfo 'BigQuery
  -> G.Name
  -> UpdPermInfo 'BigQuery
  -> Maybe (SelPermInfo 'BigQuery)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
msBuildTableUpdateMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _updPerns _selPerms =
  pure []

msBuildTableDeleteMutationFields
  :: MonadBuildSchema 'BigQuery r m n
  => SourceName
  -> SourceConfig 'BigQuery
  -> TableName 'BigQuery
  -> TableInfo 'BigQuery
  -> G.Name
  -> DelPermInfo 'BigQuery
  -> Maybe (SelPermInfo 'BigQuery)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
msBuildTableDeleteMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []

msBuildFunctionQueryFields
    :: MonadBuildSchema 'BigQuery r m n
    => SourceName
    -> SourceConfig 'BigQuery
    -> FunctionName 'BigQuery
    -> FunctionInfo 'BigQuery
    -> TableName 'BigQuery
    -> SelPermInfo 'BigQuery
    -> m [FieldParser n (QueryRootField UnpreparedValue)]
msBuildFunctionQueryFields _ _ _ _ _ _ =
  pure []

msBuildFunctionRelayQueryFields
  :: MonadBuildSchema 'BigQuery r m n
  => SourceName
  -> SourceConfig 'BigQuery
  -> FunctionName 'BigQuery
  -> FunctionInfo 'BigQuery
  -> TableName    'BigQuery
  -> NESeq (ColumnInfo 'BigQuery)
  -> SelPermInfo  'BigQuery
  -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
msBuildFunctionRelayQueryFields _sourceName _sourceInfo _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure Nothing

msBuildFunctionMutationFields
    :: MonadBuildSchema 'BigQuery r m n
    => SourceName
    -> SourceConfig 'BigQuery
    -> FunctionName 'BigQuery
    -> FunctionInfo 'BigQuery
    -> TableName 'BigQuery
    -> SelPermInfo 'BigQuery
    -> m [FieldParser n (MutationRootField UnpreparedValue)]
msBuildFunctionMutationFields _ _ _ _ _ _ =
  pure []


----------------------------------------------------------------
-- Individual components

msColumnParser
  :: (MonadSchema n m, MonadError QErr m)
  => ColumnType 'BigQuery
  -> G.Nullability
  -> m (Parser 'Both n (Opaque (ColumnValue 'BigQuery)))
msColumnParser columnType (G.Nullability isNullable) =
  opaque . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> case scalarType of
      -- bytestrings
      -- we only accept string literals
      BigQuery.BytesScalarType  -> pure $ possiblyNullable scalarType $  BigQuery.StringValue <$> P.string
      -- text
      BigQuery.StringScalarType -> pure $ possiblyNullable scalarType $  BigQuery.StringValue <$> P.string
      -- floating point values
      -- TODO: we do not perform size checks here, meaning we would accept an
      -- out-of-bounds value as long as it can be represented by a GraphQL float; this
      -- will in all likelihood error on the BigQuery side. Do we want to handle those
      -- properly here?
      BigQuery.FloatScalarType   -> pure $ possiblyNullable scalarType $  BigQuery.FloatValue . BigQuery.doubleToFloat64 <$> P.float
      -- Int types; we cram everything into Double at the moment
      -- TODO: Distinguish between ints and doubles
      BigQuery.IntegerScalarType -> pure $ possiblyNullable scalarType $  BigQuery.IntegerValue . BigQuery.intToInt64 . round <$> P.float
      BigQuery.DecimalScalarType -> pure $ possiblyNullable scalarType $  BigQuery.DecimalValue . BigQuery.doubleToDecimal <$> P.float
      BigQuery.BigDecimalScalarType -> pure $ possiblyNullable scalarType $  BigQuery.BigDecimalValue . BigQuery.doubleToBigDecimal <$> P.float
      -- boolean type
      BigQuery.BoolScalarType -> pure $ possiblyNullable scalarType $  BigQuery.BoolValue <$> P.boolean
      BigQuery.DateScalarType -> pure $ possiblyNullable scalarType $  BigQuery.DateValue . BigQuery.Date <$> P.string
      BigQuery.DatetimeScalarType -> pure $ possiblyNullable scalarType $  BigQuery.DatetimeValue . BigQuery.Datetime <$> P.string
      BigQuery.GeographyScalarType -> pure $ possiblyNullable scalarType $  BigQuery.GeographyValue . BigQuery.Geography <$> P.string
      BigQuery.TimestampScalarType -> pure $ possiblyNullable scalarType $  BigQuery.TimestampValue . BigQuery.Timestamp <$> P.string
      ty -> throwError $ RQL.internalError $ T.pack $ "Type currently unsupported for BigQuery: " ++ show ty
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName tableName `onLeft` throwError
          let enumName = tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable BigQuery.StringScalarType $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    -- Sadly, this combinator is not sound in general, so we can’t export it
    -- for general-purpose use. If we did, someone could write this:
    --
    --   mkParameter <$> opaque do
    --     n <- int
    --     pure (mkIntColumnValue (n + 1))
    --
    -- Now we’d end up with a UVParameter that has a variable in it, so we’d
    -- parameterize over it. But when we’d reuse the plan, we wouldn’t know to
    -- increment the value by 1, so we’d use the wrong value!
    --
    -- We could theoretically solve this by retaining a reference to the parser
    -- itself and re-parsing each new value, using the saved parser, which
    -- would admittedly be neat. But it’s more complicated, and it isn’t clear
    -- that it would actually be useful, so for now we don’t support it.
    opaque :: MonadParse m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          P.GraphQLValue (G.VVariable var@Variable{ vInfo, vValue }) -> do
            typeCheck False (P.toGraphQLType $ pType parser) var
            P.mkOpaque (Just vInfo) <$> pParser parser (absurd <$> vValue)
          value -> P.mkOpaque Nothing <$> pParser parser value
      }
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe BigQuery.NullValue) . P.nullable
      | otherwise  = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'BigQuery)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.mkDefinition value (G.Description <$> description) P.EnumValueInfo
      , BigQuery.StringValue $ G.unName value
      )
    throughJSON scalarName =
      let schemaType = P.NonNullable $ P.TNamed $ P.mkDefinition scalarName Nothing P.TIScalar
      in  Parser
          { pType = schemaType
          , pParser =
            valueToJSON (P.toGraphQLType schemaType) >=>
            either (parseErrorWith ParseFailed . qeError) pure . runAesonParser J.parseJSON
          }

msJsonPathArg
  :: MonadParse n
  => ColumnType 'BigQuery
  -> InputFieldsParser n (Maybe (IR.ColumnOp 'BigQuery))
msJsonPathArg _columnType = pure Nothing

msOrderByOperators
  :: NonEmpty
      ( Definition P.EnumValueInfo
      , (BasicOrderType 'BigQuery, NullsOrderType 'BigQuery)
      )
msOrderByOperators = NE.fromList
  [ ( define $$(G.litName "asc") "in ascending order, nulls first"
    , (BigQuery.AscOrder, BigQuery.NullsFirst)
    )
  , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
    , (BigQuery.AscOrder, BigQuery.NullsFirst)
    )
  , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
    , (BigQuery.AscOrder, BigQuery.NullsLast)
    )
  , ( define $$(G.litName "desc") "in descending order, nulls last"
    , (BigQuery.DescOrder, BigQuery.NullsLast)
    )
  , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
    , (BigQuery.DescOrder, BigQuery.NullsFirst)
    )
  , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
    , (BigQuery.DescOrder, BigQuery.NullsLast)
    )
  ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo

msComparisonExps
  :: forall m n
  . (BackendSchema 'BigQuery, MonadSchema n m, MonadError QErr m)
  => ColumnType 'BigQuery
  -> m (Parser 'Input n [ComparisonExp 'BigQuery])
msComparisonExps = P.memoize 'comparisonExps $ \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  typedParser        <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar BigQuery.StringScalarType) (G.Nullability True)
  textParser         <- columnParser (ColumnScalar BigQuery.StringScalarType) (G.Nullability False)
  let name = P.getName typedParser <> $$(G.litName "_BigQuery_comparison_exp")
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


msOffsetParser :: MonadParse n => Parser 'Both n (SQLExpression 'BigQuery)
msOffsetParser =
  BigQuery.ValueExpression . BigQuery.IntegerValue . BigQuery.intToInt64 . fromIntegral <$>
  P.int

msMkCountType
  :: Maybe Bool
  -- ^ distinct values
  -> Maybe [Column 'BigQuery]
  -> CountType 'BigQuery
msMkCountType _           Nothing     = BigQuery.StarCountable
msMkCountType (Just True) (Just cols) =
  maybe BigQuery.StarCountable BigQuery.DistinctCountable $ nonEmpty cols
msMkCountType _           (Just cols) =
  maybe BigQuery.StarCountable BigQuery.NonNullFieldCountable $ nonEmpty cols

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
msTableDistinctOn
  -- :: forall m n. (BackendSchema 'BigQuery, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  :: Applicative m
  => Applicative n
  => TableName 'BigQuery
  -> SelPermInfo 'BigQuery
  -> m (InputFieldsParser n (Maybe (XDistinct 'BigQuery, NonEmpty (Column 'BigQuery))))
msTableDistinctOn _table _selectPermissions = pure (pure Nothing)

-- | Various update operators
msUpdateOperators
  -- :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  :: Applicative m
  => TableName 'BigQuery         -- ^ qualified name of the table
  -> UpdPermInfo 'BigQuery       -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(Column 'BigQuery, IR.UpdOpExpG (UnpreparedValue 'BigQuery))]))
msUpdateOperators _table _updatePermissions = pure Nothing

-- | Computed field parser.
-- Currently unsupported: returns Nothing for now.
msComputedField
  :: MonadBuildSchema 'BigQuery r m n
  => ComputedFieldInfo 'BigQuery
  -> SelPermInfo 'BigQuery
  -> m (Maybe (FieldParser n (AnnotatedField 'BigQuery)))
msComputedField _fieldInfo _selectPemissions = pure Nothing

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
msRemoteRelationshipField
  :: MonadBuildSchema 'BigQuery r m n
  => RemoteFieldInfo 'BigQuery
  -> m (Maybe [FieldParser n (AnnotatedField 'BigQuery)])
msRemoteRelationshipField _remoteFieldInfo = pure Nothing

-- | The 'node' root field of a Relay request. Relay is currently unsupported on BigQuery,
-- meaning this parser will never be called: any attempt to create this parser should
-- therefore fail.
msNode
  :: MonadBuildSchema 'BigQuery r m n
  => m ( Parser 'Output n
         ( HashMap
           ( TableName 'BigQuery)
           ( SourceName, SourceConfig 'BigQuery
           , SelPermInfo 'BigQuery
           , PrimaryKeyColumns 'BigQuery
           , AnnotatedFields 'BigQuery
           )
         )
       )
msNode = throw500 "BigQuery does not support relay; `node` should never be exposed in the schema."
