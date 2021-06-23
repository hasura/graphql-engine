{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.Schema () where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                         as Map
import qualified Data.List.NonEmpty                          as NE
import qualified Database.ODBC.SQLServer                     as ODBC
import qualified Language.GraphQL.Draft.Syntax               as G

import           Data.Has
import           Data.Text.Encoding                          (encodeUtf8)
import           Data.Text.Extended

import qualified Hasura.Backends.MSSQL.Types                 as MSSQL
import qualified Hasura.GraphQL.Parser                       as P
import qualified Hasura.GraphQL.Schema.Build                 as GSB
import qualified Hasura.RQL.IR.Select                        as IR
import qualified Hasura.RQL.IR.Update                        as IR

import           Hasura.Base.Error
import           Hasura.GraphQL.Parser                       hiding (EnumValueInfo, field)
import           Hasura.GraphQL.Parser.Internal.Parser       hiding (field)
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.RQL.IR
import           Hasura.RQL.Types


----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'MSSQL where
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
  relayExtension    = Nothing
  nodesAggExtension = Just ()

  -- table arguments
  tableArguments = msTableArgs

  -- individual components
  columnParser              = msColumnParser
  jsonPathArg               = msJsonPathArg
  orderByOperators          = msOrderByOperators
  comparisonExps            = msComparisonExps
  updateOperators           = msUpdateOperators
  mkCountType               = msMkCountType
  aggregateOrderByCountType = MSSQL.IntegerType
  computedField             = msComputedField
  node                      = msNode

  -- SQL literals
  columnDefaultValue = msColumnDefaultValue


----------------------------------------------------------------
-- Top level parsers

msBuildTableRelayQueryFields
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> SourceConfig 'MSSQL
  -> TableName    'MSSQL
  -> TableInfo    'MSSQL
  -> G.Name
  -> NESeq (ColumnInfo 'MSSQL)
  -> SelPermInfo  'MSSQL
  -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
msBuildTableRelayQueryFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []

msBuildTableInsertMutationFields
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> SourceConfig 'MSSQL
  -> TableName 'MSSQL
  -> TableInfo 'MSSQL
  -> G.Name
  -> InsPermInfo 'MSSQL
  -> Maybe (SelPermInfo 'MSSQL)
  -> Maybe (UpdPermInfo 'MSSQL)
  -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
msBuildTableInsertMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _insPerms _selPerms _updPerms =
  pure []

msBuildTableUpdateMutationFields
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> SourceConfig 'MSSQL
  -> TableName 'MSSQL
  -> TableInfo 'MSSQL
  -> G.Name
  -> UpdPermInfo 'MSSQL
  -> Maybe (SelPermInfo 'MSSQL)
  -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
msBuildTableUpdateMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _updPerns _selPerms =
  pure []

msBuildTableDeleteMutationFields
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> SourceConfig 'MSSQL
  -> TableName 'MSSQL
  -> TableInfo 'MSSQL
  -> G.Name
  -> DelPermInfo 'MSSQL
  -> Maybe (SelPermInfo 'MSSQL)
  -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
msBuildTableDeleteMutationFields _sourceName _sourceInfo _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []

msBuildFunctionQueryFields
    :: MonadBuildSchema 'MSSQL r m n
    => SourceName
    -> SourceConfig 'MSSQL
    -> FunctionName 'MSSQL
    -> FunctionInfo 'MSSQL
    -> TableName 'MSSQL
    -> SelPermInfo 'MSSQL
    -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
msBuildFunctionQueryFields _ _ _ _ _ _ =
  pure []

msBuildFunctionRelayQueryFields
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> SourceConfig 'MSSQL
  -> FunctionName 'MSSQL
  -> FunctionInfo 'MSSQL
  -> TableName    'MSSQL
  -> NESeq (ColumnInfo 'MSSQL)
  -> SelPermInfo  'MSSQL
  -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
msBuildFunctionRelayQueryFields _sourceName _sourceInfo _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure []

msBuildFunctionMutationFields
    :: MonadBuildSchema 'MSSQL r m n
    => SourceName
    -> SourceConfig 'MSSQL
    -> FunctionName 'MSSQL
    -> FunctionInfo 'MSSQL
    -> TableName 'MSSQL
    -> SelPermInfo 'MSSQL
    -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
msBuildFunctionMutationFields _ _ _ _ _ _ =
  pure []


----------------------------------------------------------------
-- Table arguments

msTableArgs
  :: forall r m n
   . MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> TableInfo 'MSSQL
  -> SelPermInfo 'MSSQL
  -> m (InputFieldsParser n (IR.SelectArgsG 'MSSQL (UnpreparedValue 'MSSQL)))
msTableArgs sourceName tableInfo selectPermissions = do
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
      -- not supported on MSSQL for now
      , IR._saDistinct = Nothing
      }


----------------------------------------------------------------
-- Individual components

msColumnParser
  :: (MonadSchema n m, MonadError QErr m)
  => ColumnType 'MSSQL
  -> G.Nullability
  -> m (Parser 'Both n (Opaque (ColumnValue 'MSSQL)))
msColumnParser columnType (G.Nullability isNullable) =
  opaque . fmap (ColumnValue columnType) <$> case columnType of
    -- TODO: the mapping here is not consistent with mkMSSQLScalarTypeName. For
    -- example, exposing all the float types as a GraphQL Float type is
    -- incorrect, similarly exposing all the integer types as a GraphQL Int
    ColumnScalar scalarType -> possiblyNullable scalarType <$> case scalarType of
      -- bytestring
      MSSQL.CharType     -> pure $ ODBC.ByteStringValue . encodeUtf8 <$> P.string
      MSSQL.VarcharType  -> pure $ ODBC.ByteStringValue . encodeUtf8 <$> P.string

      -- text
      MSSQL.WcharType    -> pure $ ODBC.TextValue <$> P.string
      MSSQL.WvarcharType -> pure $ ODBC.TextValue <$> P.string
      MSSQL.WtextType    -> pure $ ODBC.TextValue <$> P.string
      MSSQL.TextType     -> pure $ ODBC.TextValue <$> P.string

      -- integer
      MSSQL.IntegerType  -> pure $ ODBC.IntValue . fromIntegral <$> P.int
      MSSQL.SmallintType -> pure $ ODBC.IntValue . fromIntegral <$> P.int
      MSSQL.BigintType   -> pure $ ODBC.IntValue . fromIntegral <$> P.int
      MSSQL.TinyintType  -> pure $ ODBC.IntValue . fromIntegral <$> P.int

      -- float
      MSSQL.NumericType  -> pure $ ODBC.DoubleValue <$> P.float
      MSSQL.DecimalType  -> pure $ ODBC.DoubleValue <$> P.float
      MSSQL.FloatType    -> pure $ ODBC.DoubleValue <$> P.float
      MSSQL.RealType     -> pure $ ODBC.DoubleValue <$> P.float

      -- boolean
      MSSQL.BitType      -> pure $ ODBC.BoolValue <$> P.boolean
      _                  -> do
        name <- MSSQL.mkMSSQLScalarTypeName scalarType
        let schemaType = P.NonNullable $ P.TNamed $ P.mkDefinition name Nothing P.TIScalar
        pure $ Parser
          { pType = schemaType
          , pParser =
              valueToJSON (P.toGraphQLType schemaType) >=>
              either (parseErrorWith ParseFailed . qeError) pure . (MSSQL.parseScalarValue scalarType)
          }
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName @'MSSQL tableName `onLeft` throwError
          let enumName = tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable MSSQL.VarcharType $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
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
      | isNullable = fmap (fromMaybe ODBC.NullValue) . P.nullable
      | otherwise  = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'MSSQL)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.mkDefinition value (G.Description <$> description) P.EnumValueInfo
      , ODBC.TextValue $ G.unName value
      )

msJsonPathArg
  :: MonadParse n
  => ColumnType 'MSSQL
  -> InputFieldsParser n (Maybe (IR.ColumnOp 'MSSQL))
msJsonPathArg _columnType = pure Nothing

msOrderByOperators
  :: NonEmpty
      ( Definition P.EnumValueInfo
      , (BasicOrderType 'MSSQL, NullsOrderType 'MSSQL)
      )
msOrderByOperators = NE.fromList
  [ ( define $$(G.litName "asc") "in ascending order, nulls first"
    , (MSSQL.AscOrder, MSSQL.NullsFirst)
    )
  , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
    , (MSSQL.AscOrder, MSSQL.NullsFirst)
    )
  , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
    , (MSSQL.AscOrder, MSSQL.NullsLast)
    )
  , ( define $$(G.litName "desc") "in descending order, nulls last"
    , (MSSQL.DescOrder, MSSQL.NullsLast)
    )
  , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
    , (MSSQL.DescOrder, MSSQL.NullsFirst)
    )
  , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
    , (MSSQL.DescOrder, MSSQL.NullsLast)
    )
  ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo

msComparisonExps
  :: forall m n r
   . ( BackendSchema 'MSSQL
     , MonadSchema n m
     , MonadError QErr m
     , MonadReader r m
     , Has QueryContext r
     )
  => ColumnType 'MSSQL
  -> m (Parser 'Input n [ComparisonExp 'MSSQL])
msComparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- asks $ qcDangerousBooleanCollapse . getter

  -- parsers used for individual values
  typedParser        <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability True)
  textParser         <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability False)
  let
    columnListParser = P.list typedParser `P.bind` traverse P.openOpaque
    textListParser   = P.list textParser  `P.bind` traverse P.openOpaque

  -- field info
  let
    name = P.getName typedParser <> $$(G.litName "_MSSQL_comparison_exp")
    desc = G.Description $ "Boolean expression to compare columns of type "
      <>  P.getName typedParser
      <<> ". All fields are combined with logical 'AND'."

  pure $ P.object name (Just desc) $ fmap catMaybes $ sequenceA $ concat
    [
    -- Common ops for all types
      equalityOperators
        collapseIfNull
        (mkParameter <$> typedParser)
        (mkListLiteral <$> columnListParser)
    , comparisonOperators
        collapseIfNull
        (mkParameter <$> typedParser)

    -- Ops for String like types
    , guard (isScalarColumnWhere (`elem` MSSQL.stringTypes) columnType) *>
      [ P.fieldOptional $$(G.litName "_like")
        (Just "does the column match the given pattern")
        (ALIKE     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nlike")
        (Just "does the column NOT match the given pattern")
        (ANLIKE    . mkParameter <$> typedParser)
      ]

    -- Ops for Geometry/Geography types
    , guard (isScalarColumnWhere (`elem` MSSQL.geoTypes) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_contains")
        (Just "does the column contain the given value")
        (ABackendSpecific . MSSQL.ASTContains   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_equals")
        (Just "is the column equal to given value (directionality is ignored)")
        (ABackendSpecific . MSSQL.ASTEquals     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given value")
        (ABackendSpecific . MSSQL.ASTIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_overlaps")
        (Just "does the column 'spatially overlap' (intersect but not completely contain) the given value")
        (ABackendSpecific . MSSQL.ASTOverlaps   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_within")
        (Just "is the column contained in the given value")
        (ABackendSpecific . MSSQL.ASTWithin     . mkParameter <$> typedParser)
      ]

    -- Ops for Geometry types
    , guard (isScalarColumnWhere (MSSQL.GeometryType ==) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_crosses")
        (Just "does the column cross the given geometry value")
        (ABackendSpecific . MSSQL.ASTCrosses . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_touches")
        (Just "does the column have at least one point in common with the given geometry value")
        (ABackendSpecific . MSSQL.ASTTouches . mkParameter <$> typedParser)
      ]
    ]
  where
    mkListLiteral :: [ColumnValue 'MSSQL] -> UnpreparedValue 'MSSQL
    mkListLiteral =
      P.UVLiteral . MSSQL.ListExpression . fmap (MSSQL.ValueExpression . cvValue)

msMkCountType
  :: Maybe Bool
  -- ^ distinct values
  -> Maybe [Column 'MSSQL]
  -> CountType 'MSSQL
msMkCountType _           Nothing     = MSSQL.StarCountable
msMkCountType (Just True) (Just cols) =
  maybe MSSQL.StarCountable MSSQL.DistinctCountable $ nonEmpty cols
msMkCountType _           (Just cols) =
  maybe MSSQL.StarCountable MSSQL.NonNullFieldCountable $ nonEmpty cols

-- | Various update operators
msUpdateOperators
  :: Applicative m
  => TableInfo 'MSSQL         -- ^ table info
  -> UpdPermInfo 'MSSQL       -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(Column 'MSSQL, IR.UpdOpExpG (UnpreparedValue 'MSSQL))]))
msUpdateOperators _tableInfo _updatePermissions = pure Nothing

-- | Computed field parser.
-- Currently unsupported: returns Nothing for now.
msComputedField
  :: MonadBuildSchema 'MSSQL r m n
  => SourceName
  -> ComputedFieldInfo 'MSSQL
  -> TableName 'MSSQL
  -> SelPermInfo 'MSSQL
  -> m (Maybe (FieldParser n (AnnotatedField 'MSSQL)))
msComputedField _sourceName _fieldInfo _table _selectPemissions = pure Nothing

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
msRemoteRelationshipField
  :: MonadBuildSchema 'MSSQL r m n
  => RemoteFieldInfo 'MSSQL
  -> m (Maybe [FieldParser n (AnnotatedField 'MSSQL)])
msRemoteRelationshipField _remoteFieldInfo = pure Nothing

-- | The 'node' root field of a Relay request. Relay is currently unsupported on MSSQL,
-- meaning this parser will never be called: any attempt to create this parser should
-- therefore fail.
msNode
  :: MonadBuildSchema 'MSSQL r m n
  => m ( Parser 'Output n
         ( HashMap
           ( TableName 'MSSQL)
           ( SourceName, SourceConfig 'MSSQL
           , SelPermInfo 'MSSQL
           , PrimaryKeyColumns 'MSSQL
           , AnnotatedFields 'MSSQL
           )
         )
       )
msNode = throw500 "MSSQL does not support relay; `node` should never be exposed in the schema."


----------------------------------------------------------------
-- SQL literals

-- FIXME: this is nonsensical for MSSQL, we'll need to adjust the corresponding mutation
-- and its representation.
msColumnDefaultValue :: Column 'MSSQL -> SQLExpression 'MSSQL
msColumnDefaultValue = const $ MSSQL.ValueExpression ODBC.NullValue
