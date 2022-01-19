{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Schema
--
-- Defines a 'Hasura.GraphQL.Schema.Backend.BackendSchema' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Schema () where

import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Schema.IfMatched
import Hasura.Backends.MSSQL.Types.Insert (BackendInsert (..))
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Backends.MSSQL.Types.Update (BackendUpdate (..), UpdateOperator (..))
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Update qualified as SU
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types hiding (BackendInsert)
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------

-- * BackendSchema instance

instance BackendSchema 'MSSQL where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields
  buildTableRelayQueryFields = msBuildTableRelayQueryFields
  buildTableInsertMutationFields =
    GSB.buildTableInsertMutationFields backendInsertParser
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildTableUpdateMutationFields = msBuildTableUpdateMutationFields

  buildFunctionQueryFields = msBuildFunctionQueryFields
  buildFunctionRelayQueryFields = msBuildFunctionRelayQueryFields
  buildFunctionMutationFields = msBuildFunctionMutationFields

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()

  -- table arguments
  tableArguments = msTableArgs
  mkRelationshipParser = msMkRelationshipParser

  -- individual components
  columnParser = msColumnParser
  jsonPathArg = msJsonPathArg
  orderByOperators = msOrderByOperators
  comparisonExps = msComparisonExps
  countTypeInput = msCountTypeInput
  aggregateOrderByCountType = MSSQL.IntegerType
  computedField = msComputedField
  node = msNode

  -- SQL literals
  columnDefaultValue = msColumnDefaultValue

----------------------------------------------------------------

-- * Top level parsers

msBuildTableRelayQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  G.Name ->
  NESeq (ColumnInfo 'MSSQL) ->
  SelPermInfo 'MSSQL ->
  m [a]
msBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []

backendInsertParser ::
  forall m r n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  Maybe (UpdPermInfo 'MSSQL) ->
  m (InputFieldsParser n (BackendInsert (UnpreparedValue 'MSSQL)))
backendInsertParser sourceName tableInfo selectPerms updatePerms = do
  ifMatched <- ifMatchedFieldParser sourceName tableInfo selectPerms updatePerms
  let _biIdentityColumns = _tciExtraTableMetadata $ _tiCoreInfo tableInfo
  pure $ do
    _biIfMatched <- ifMatched
    pure $ BackendInsert {..}

msBuildTableUpdateMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  G.Name ->
  UpdPermInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  m [FieldParser n (AnnotatedUpdateG 'MSSQL (RemoteRelationshipField UnpreparedValue) (UnpreparedValue 'MSSQL))]
msBuildTableUpdateMutationFields =
  GSB.buildTableUpdateMutationFields
    ( \ti updPerms ->
        fmap BackendUpdate
          <$> SU.buildUpdateOperators
            (UpdateSet <$> SU.presetColumns updPerms)
            [ UpdateSet <$> SU.setOp,
              UpdateInc <$> SU.incOp
            ]
            ti
            updPerms
    )

msBuildTableDeleteMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  G.Name ->
  DelPermInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  m [a]
msBuildTableDeleteMutationFields _sourceName _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []

msBuildFunctionQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  SelPermInfo 'MSSQL ->
  m [a]
msBuildFunctionQueryFields _ _ _ _ _ =
  pure []

msBuildFunctionRelayQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  NESeq (ColumnInfo 'MSSQL) ->
  SelPermInfo 'MSSQL ->
  m [a]
msBuildFunctionRelayQueryFields _sourceName _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure []

msBuildFunctionMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  SelPermInfo 'MSSQL ->
  m [a]
msBuildFunctionMutationFields _ _ _ _ _ =
  pure []

----------------------------------------------------------------

-- * Table arguments

msTableArgs ::
  forall r m n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableInfo 'MSSQL ->
  SelPermInfo 'MSSQL ->
  m (InputFieldsParser n (IR.SelectArgsG 'MSSQL (UnpreparedValue 'MSSQL)))
msTableArgs sourceName tableInfo selectPermissions = do
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
          -- not supported on MSSQL for now
          IR._saDistinct = Nothing
        }

msMkRelationshipParser ::
  forall r m n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  RelInfo 'MSSQL ->
  m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert 'MSSQL (UnpreparedValue 'MSSQL)))))
msMkRelationshipParser _sourceName _relationshipInfo = do
  -- When we support nested inserts, we also need to ensure we limit ourselves
  -- to inserting into tables whch supports inserts:
  {-
    import Hasura.GraphQL.Schema.Mutation qualified as GSB

    runMaybeT $ do
      let otherTableName = riRTable relationshipInfo
      otherTableInfo <- lift $ askTableInfo sourceName otherTableName
      guard (supportsInserts otherTableInfo)
  -}
  return Nothing

----------------------------------------------------------------

-- * Individual components

msColumnParser ::
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  ColumnType 'MSSQL ->
  G.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (ColumnValue 'MSSQL)))
msColumnParser columnType (G.Nullability isNullable) =
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    -- TODO: the mapping here is not consistent with mkMSSQLScalarTypeName. For
    -- example, exposing all the float types as a GraphQL Float type is
    -- incorrect, similarly exposing all the integer types as a GraphQL Int
    ColumnScalar scalarType ->
      possiblyNullable scalarType <$> case scalarType of
        -- bytestring
        MSSQL.CharType -> pure $ ODBC.ByteStringValue . encodeUtf8 <$> P.string
        MSSQL.VarcharType -> pure $ ODBC.ByteStringValue . encodeUtf8 <$> P.string
        -- text
        MSSQL.WcharType -> pure $ ODBC.TextValue <$> P.string
        MSSQL.WvarcharType -> pure $ ODBC.TextValue <$> P.string
        MSSQL.WtextType -> pure $ ODBC.TextValue <$> P.string
        MSSQL.TextType -> pure $ ODBC.TextValue <$> P.string
        -- integer
        MSSQL.IntegerType -> pure $ ODBC.IntValue . fromIntegral <$> P.int
        MSSQL.SmallintType -> pure $ ODBC.IntValue . fromIntegral <$> P.int
        MSSQL.BigintType -> pure $ ODBC.IntValue . fromIntegral <$> P.int
        MSSQL.TinyintType -> pure $ ODBC.IntValue . fromIntegral <$> P.int
        -- float
        MSSQL.NumericType -> pure $ ODBC.DoubleValue <$> P.float
        MSSQL.DecimalType -> pure $ ODBC.DoubleValue <$> P.float
        MSSQL.FloatType -> pure $ ODBC.DoubleValue <$> P.float
        MSSQL.RealType -> pure $ ODBC.DoubleValue <$> P.float
        -- boolean
        MSSQL.BitType -> pure $ ODBC.BoolValue <$> P.boolean
        _ -> do
          name <- MSSQL.mkMSSQLScalarTypeName scalarType
          let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing P.TIScalar
          pure $
            Parser
              { pType = schemaType,
                pParser =
                  valueToJSON (P.toGraphQLType schemaType)
                    >=> either (parseErrorWith ParseFailed . qeError) pure . (MSSQL.parseScalarValue scalarType)
              }
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName @'MSSQL tableName `onLeft` throwError
          enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable MSSQL.VarcharType $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe ODBC.NullValue) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'MSSQL)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) P.EnumValueInfo,
        ODBC.TextValue $ G.unName value
      )

msJsonPathArg ::
  MonadParse n =>
  ColumnType 'MSSQL ->
  InputFieldsParser n (Maybe (IR.ColumnOp 'MSSQL))
msJsonPathArg _columnType = pure Nothing

msOrderByOperators ::
  NonEmpty
    ( Definition P.EnumValueInfo,
      (BasicOrderType 'MSSQL, NullsOrderType 'MSSQL)
    )
msOrderByOperators =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls first",
        (MSSQL.AscOrder, MSSQL.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first",
        (MSSQL.AscOrder, MSSQL.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last",
        (MSSQL.AscOrder, MSSQL.NullsLast)
      ),
      ( define $$(G.litName "desc") "in descending order, nulls last",
        (MSSQL.DescOrder, MSSQL.NullsLast)
      ),
      ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first",
        (MSSQL.DescOrder, MSSQL.NullsFirst)
      ),
      ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last",
        (MSSQL.DescOrder, MSSQL.NullsLast)
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

msComparisonExps ::
  forall m n r.
  ( BackendSchema 'MSSQL,
    MonadSchema n m,
    MonadError QErr m,
    MonadReader r m,
    Has QueryContext r,
    Has MkTypename r
  ) =>
  ColumnType 'MSSQL ->
  m (Parser 'Input n [ComparisonExp 'MSSQL])
msComparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- asks $ qcDangerousBooleanCollapse . getter

  -- parsers used for individual values
  typedParser <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability True)
  textParser <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability False)
  let columnListParser = fmap openValueOrigin <$> P.list typedParser
      textListParser = fmap openValueOrigin <$> P.list textParser

  -- field info
  let name = P.getName typedParser <> $$(G.litName "_MSSQL_comparison_exp")
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."

  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ -- Common ops for all types
              equalityOperators
                collapseIfNull
                (mkParameter <$> typedParser)
                (mkListLiteral <$> columnListParser),
              comparisonOperators
                collapseIfNull
                (mkParameter <$> typedParser),
              -- Ops for String like types
              guard (isScalarColumnWhere (`elem` MSSQL.stringTypes) columnType)
                *> [ P.fieldOptional
                       $$(G.litName "_like")
                       (Just "does the column match the given pattern")
                       (ALIKE . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_nlike")
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . mkParameter <$> typedParser)
                   ],
              -- Ops for Geometry/Geography types
              guard (isScalarColumnWhere (`elem` MSSQL.geoTypes) columnType)
                *> [ P.fieldOptional
                       $$(G.litName "_st_contains")
                       (Just "does the column contain the given value")
                       (ABackendSpecific . MSSQL.ASTContains . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_st_equals")
                       (Just "is the column equal to given value (directionality is ignored)")
                       (ABackendSpecific . MSSQL.ASTEquals . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_st_intersects")
                       (Just "does the column spatially intersect the given value")
                       (ABackendSpecific . MSSQL.ASTIntersects . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_st_overlaps")
                       (Just "does the column 'spatially overlap' (intersect but not completely contain) the given value")
                       (ABackendSpecific . MSSQL.ASTOverlaps . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_st_within")
                       (Just "is the column contained in the given value")
                       (ABackendSpecific . MSSQL.ASTWithin . mkParameter <$> typedParser)
                   ],
              -- Ops for Geometry types
              guard (isScalarColumnWhere (MSSQL.GeometryType ==) columnType)
                *> [ P.fieldOptional
                       $$(G.litName "_st_crosses")
                       (Just "does the column cross the given geometry value")
                       (ABackendSpecific . MSSQL.ASTCrosses . mkParameter <$> typedParser),
                     P.fieldOptional
                       $$(G.litName "_st_touches")
                       (Just "does the column have at least one point in common with the given geometry value")
                       (ABackendSpecific . MSSQL.ASTTouches . mkParameter <$> typedParser)
                   ]
            ]
  where
    mkListLiteral :: [ColumnValue 'MSSQL] -> UnpreparedValue 'MSSQL
    mkListLiteral =
      P.UVLiteral . MSSQL.ListExpression . fmap (MSSQL.ValueExpression . cvValue)

msCountTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'MSSQL)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'MSSQL)
msCountTypeInput = \case
  Just columnEnum -> do
    column <- P.fieldOptional $$(G.litName "column") Nothing columnEnum
    pure $ flip mkCountType column
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe (Column 'MSSQL) -> CountType 'MSSQL
    mkCountType _ Nothing = MSSQL.StarCountable
    mkCountType IR.SelectCountDistinct (Just col) = MSSQL.DistinctCountable col
    mkCountType IR.SelectCountNonDistinct (Just col) = MSSQL.NonNullFieldCountable col

-- | Computed field parser.
-- Currently unsupported: returns Nothing for now.
msComputedField ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  ComputedFieldInfo 'MSSQL ->
  TableName 'MSSQL ->
  SelPermInfo 'MSSQL ->
  m (Maybe (FieldParser n (AnnotatedField 'MSSQL)))
msComputedField _sourceName _fieldInfo _table _selectPemissions = pure Nothing

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
msRemoteRelationshipField ::
  MonadBuildSchema 'MSSQL r m n =>
  RemoteFieldInfo (DBJoinField 'MSSQL) ->
  m (Maybe [FieldParser n (AnnotatedField 'MSSQL)])
msRemoteRelationshipField _remoteFieldInfo = pure Nothing

-- | The 'node' root field of a Relay request. Relay is currently unsupported on MSSQL,
-- meaning this parser will never be called: any attempt to create this parser should
-- therefore fail.
msNode ::
  MonadBuildSchema 'MSSQL r m n =>
  m
    ( Parser
        'Output
        n
        ( HashMap
            (TableName 'MSSQL)
            ( SourceName,
              SourceConfig 'MSSQL,
              SelPermInfo 'MSSQL,
              PrimaryKeyColumns 'MSSQL,
              AnnotatedFields 'MSSQL
            )
        )
    )
msNode = throw500 "MSSQL does not support relay; `node` should never be exposed in the schema."

----------------------------------------------------------------

-- * SQL literals

-- FIXME: this is nonsensical for MSSQL, we'll need to adjust the corresponding mutation
-- and its representation.
msColumnDefaultValue :: Column 'MSSQL -> SQLExpression 'MSSQL
msColumnDefaultValue = const $ MSSQL.ValueExpression ODBC.NullValue
