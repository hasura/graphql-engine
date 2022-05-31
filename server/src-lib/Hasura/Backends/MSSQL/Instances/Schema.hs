{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Schema
--
-- Defines a 'Hasura.GraphQL.Schema.Backend.BackendSchema' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Schema () where

import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Schema.IfMatched
import Hasura.Backends.MSSQL.Types.Insert (BackendInsert (..))
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Backends.MSSQL.Types.Update (BackendUpdate (..), UpdateOperator (..))
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Update qualified as SU
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Backend hiding (BackendInsert)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (NamingCase)
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------

-- * BackendSchema instance

instance BackendSchema 'MSSQL where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields
  buildTableRelayQueryFields = msBuildTableRelayQueryFields
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields = GSB.buildTableInsertMutationFields backendInsertParser
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildTableUpdateMutationFields = msBuildTableUpdateMutationFields

  buildFunctionQueryFields = msBuildFunctionQueryFields
  buildFunctionRelayQueryFields = msBuildFunctionRelayQueryFields
  buildFunctionMutationFields = msBuildFunctionMutationFields

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing

  -- table arguments
  tableArguments = msTableArgs
  mkRelationshipParser = msMkRelationshipParser

  -- individual components
  columnParser = msColumnParser
  scalarSelectionArgumentsParser = msScalarSelectionArgumentsParser
  orderByOperators = msOrderByOperators
  comparisonExps = msComparisonExps
  countTypeInput = msCountTypeInput
  aggregateOrderByCountType = MSSQL.IntegerType
  computedField = msComputedField
  node = msNode

----------------------------------------------------------------

-- * Top level parsers

msBuildTableRelayQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  C.GQLNameIdentifier ->
  NESeq (ColumnInfo 'MSSQL) ->
  m [a]
msBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns =
  pure []

backendInsertParser ::
  forall m r n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  TableInfo 'MSSQL ->
  m (InputFieldsParser n (BackendInsert (UnpreparedValue 'MSSQL)))
backendInsertParser sourceName tableInfo = do
  ifMatched <- ifMatchedFieldParser sourceName tableInfo
  let _biIdentityColumns = _tciExtraTableMetadata $ _tiCoreInfo tableInfo
  pure $ do
    _biIfMatched <- ifMatched
    pure $ BackendInsert {..}

msBuildTableUpdateMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  Scenario ->
  SourceInfo 'MSSQL ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  C.GQLNameIdentifier ->
  m [FieldParser n (AnnotatedUpdateG 'MSSQL (RemoteRelationshipField UnpreparedValue) (UnpreparedValue 'MSSQL))]
msBuildTableUpdateMutationFields scenario sourceName tableName tableInfo gqlName = do
  fieldParsers <- runMaybeT do
    updatePerms <- MaybeT $ _permUpd <$> tablePermissions tableInfo
    let mkBackendUpdate backendUpdateTableInfo =
          (fmap . fmap) BackendUpdate $
            SU.buildUpdateOperators
              (UpdateSet <$> SU.presetColumns updatePerms)
              [ UpdateSet <$> SU.setOp,
                UpdateInc <$> SU.incOp
              ]
              backendUpdateTableInfo
    lift $
      GSB.buildTableUpdateMutationFields
        mkBackendUpdate
        scenario
        sourceName
        tableName
        tableInfo
        gqlName
  pure . fold @Maybe @[_] $ fieldParsers

{-
NOTE: We currently use 'GSB.buildTableDeleteMutationFields' instead of
this. Should we save it?

msBuildTableDeleteMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  G.Name ->
  DelPermInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  m [a]
msBuildTableDeleteMutationFields _sourceName _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []
-}

msBuildFunctionQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  m [a]
msBuildFunctionQueryFields _ _ _ _ =
  pure []

msBuildFunctionRelayQueryFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  NESeq (ColumnInfo 'MSSQL) ->
  m [a]
msBuildFunctionRelayQueryFields _sourceName _functionName _functionInfo _tableName _pkeyColumns =
  pure []

msBuildFunctionMutationFields ::
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  FunctionName 'MSSQL ->
  FunctionInfo 'MSSQL ->
  TableName 'MSSQL ->
  m [a]
msBuildFunctionMutationFields _ _ _ _ =
  pure []

----------------------------------------------------------------

-- * Table arguments

msTableArgs ::
  forall r m n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceInfo 'MSSQL ->
  TableInfo 'MSSQL ->
  m (InputFieldsParser n (IR.SelectArgsG 'MSSQL (UnpreparedValue 'MSSQL)))
msTableArgs sourceName tableInfo = do
  whereParser <- tableWhereArg sourceName tableInfo
  orderByParser <- tableOrderByArg sourceName tableInfo
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
  SourceInfo 'MSSQL ->
  RelInfo 'MSSQL ->
  m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField 'MSSQL (UnpreparedValue 'MSSQL)))))
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
        -- text
        MSSQL.CharType -> pure $ ODBC.TextValue <$> P.string
        MSSQL.VarcharType -> pure $ ODBC.TextValue <$> P.string
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
    ColumnEnumReference enumRef@(EnumReference _ enumValues _) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          enumName <- mkEnumTypeName enumRef
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

msScalarSelectionArgumentsParser ::
  MonadParse n =>
  ColumnType 'MSSQL ->
  InputFieldsParser n (Maybe (ScalarSelectionArguments 'MSSQL))
msScalarSelectionArgumentsParser _columnType = pure Nothing

msOrderByOperators ::
  NamingCase ->
  NonEmpty
    ( Definition P.EnumValueInfo,
      (BasicOrderType 'MSSQL, NullsOrderType 'MSSQL)
    )
msOrderByOperators _tCase =
  -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
  NE.fromList
    [ ( define G._asc "in ascending order, nulls first",
        (MSSQL.AscOrder, MSSQL.NullsFirst)
      ),
      ( define G._asc_nulls_first "in ascending order, nulls first",
        (MSSQL.AscOrder, MSSQL.NullsFirst)
      ),
      ( define G._asc_nulls_last "in ascending order, nulls last",
        (MSSQL.AscOrder, MSSQL.NullsLast)
      ),
      ( define G._desc "in descending order, nulls last",
        (MSSQL.DescOrder, MSSQL.NullsLast)
      ),
      ( define G._desc_nulls_first "in descending order, nulls first",
        (MSSQL.DescOrder, MSSQL.NullsFirst)
      ),
      ( define G._desc_nulls_last "in descending order, nulls last",
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
    Has SchemaOptions r,
    Has MkTypename r,
    Has NamingCase r
  ) =>
  ColumnType 'MSSQL ->
  m (Parser 'Input n [ComparisonExp 'MSSQL])
msComparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- retrieve soDangerousBooleanCollapse

  -- parsers used for individual values
  typedParser <- columnParser columnType (G.Nullability False)
  _nullableTextParser <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability True)
  textParser <- columnParser (ColumnScalar @'MSSQL MSSQL.VarcharType) (G.Nullability False)
  let columnListParser = fmap openValueOrigin <$> P.list typedParser
      _textListParser = fmap openValueOrigin <$> P.list textParser

  -- field info
  let name = P.getName typedParser <> G.__MSSQL_comparison_exp
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."

  -- Naming convention
  tCase <- asks getter

  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ -- Common ops for all types
              equalityOperators
                tCase
                collapseIfNull
                (mkParameter <$> typedParser)
                (mkListLiteral <$> columnListParser),
              comparisonOperators
                tCase
                collapseIfNull
                (mkParameter <$> typedParser),
              -- Ops for String like types
              guard (isScalarColumnWhere (`elem` MSSQL.stringTypes) columnType)
                *> [ P.fieldOptional
                       G.__like
                       (Just "does the column match the given pattern")
                       (ALIKE . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__nlike
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . mkParameter <$> typedParser)
                   ],
              -- Ops for Geometry/Geography types
              guard (isScalarColumnWhere (`elem` MSSQL.geoTypes) columnType)
                *> [ P.fieldOptional
                       G.__st_contains
                       (Just "does the column contain the given value")
                       (ABackendSpecific . MSSQL.ASTContains . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__st_equals
                       (Just "is the column equal to given value (directionality is ignored)")
                       (ABackendSpecific . MSSQL.ASTEquals . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__st_intersects
                       (Just "does the column spatially intersect the given value")
                       (ABackendSpecific . MSSQL.ASTIntersects . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__st_overlaps
                       (Just "does the column 'spatially overlap' (intersect but not completely contain) the given value")
                       (ABackendSpecific . MSSQL.ASTOverlaps . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__st_within
                       (Just "is the column contained in the given value")
                       (ABackendSpecific . MSSQL.ASTWithin . mkParameter <$> typedParser)
                   ],
              -- Ops for Geometry types
              guard (isScalarColumnWhere (MSSQL.GeometryType ==) columnType)
                *> [ P.fieldOptional
                       G.__st_crosses
                       (Just "does the column cross the given geometry value")
                       (ABackendSpecific . MSSQL.ASTCrosses . mkParameter <$> typedParser),
                     P.fieldOptional
                       G.__st_touches
                       (Just "does the column have at least one point in common with the given geometry value")
                       (ABackendSpecific . MSSQL.ASTTouches . mkParameter <$> typedParser)
                   ]
            ]
  where
    mkListLiteral :: [ColumnValue 'MSSQL] -> UnpreparedValue 'MSSQL
    mkListLiteral =
      UVLiteral . MSSQL.ListExpression . fmap (MSSQL.ValueExpression . cvValue)

msCountTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'MSSQL)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'MSSQL)
msCountTypeInput = \case
  Just columnEnum -> do
    column <- P.fieldOptional G._column Nothing columnEnum
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
  SourceInfo 'MSSQL ->
  ComputedFieldInfo 'MSSQL ->
  TableName 'MSSQL ->
  TableInfo 'MSSQL ->
  m (Maybe (FieldParser n (AnnotatedField 'MSSQL)))
msComputedField _sourceName _fieldInfo _table _tableInfo = pure Nothing

{-
NOTE: Unused, should we remove?

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
msRemoteRelationshipField ::
  MonadBuildSchema 'MSSQL r m n =>
  RemoteFieldInfo (DBJoinField 'MSSQL) ->
  m (Maybe [FieldParser n (AnnotatedField 'MSSQL)])
msRemoteRelationshipField _remoteFieldInfo = pure Nothing
-}

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
