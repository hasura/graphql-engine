{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Schema
--
-- Defines a 'Hasura.GraphQL.Schema.Backend.BackendSchema' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Schema () where

import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding as TE
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Schema.IfMatched
import Hasura.Backends.MSSQL.Types.Insert (BackendInsert (..))
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Backends.MSSQL.Types.Update (UpdateOperator (..))
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    MonadParse,
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Update qualified as SU
import Hasura.LogicalModel.Schema (defaultLogicalModelArgs, defaultLogicalModelSelectionSet)
import Hasura.Name qualified as Name
import Hasura.NativeQuery.Schema qualified as NativeQueries
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Backend hiding (BackendInsert)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.StoredProcedure.Schema qualified as StoredProcedures
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------

-- * BackendSchema instance

instance BackendSchema 'MSSQL where
  -- top level parsers
  buildTableQueryAndSubscriptionFields = GSB.buildTableQueryAndSubscriptionFields
  buildTableRelayQueryFields _ _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields = GSB.buildTableInsertMutationFields backendInsertParser
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildTableUpdateMutationFields = GSB.buildSingleBatchTableUpdateMutationFields id
  buildNativeQueryRootFields = NativeQueries.defaultBuildNativeQueryRootFields
  buildStoredProcedureRootFields = StoredProcedures.defaultBuildStoredProcedureRootFields

  buildFunctionQueryFields _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ = pure []

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing

  -- When we support nested inserts, we also need to ensure we limit ourselves
  -- to inserting into tables whch supports inserts:
  {-
    import Hasura.GraphQL.Schema.Mutation qualified as GSB

    runMaybeT $ do
      let otherTableName = riRTable relationshipInfo
      otherTableInfo <- lift $ askTableInfo sourceName otherTableName
      guard (supportsInserts otherTableInfo)
  -}
  mkRelationshipParser _ = pure Nothing

  -- individual components
  columnParser = msColumnParser
  enumParser = msEnumParser
  possiblyNullable = msPossiblyNullable
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators _sourceInfo = msOrderByOperators
  comparisonExps = msComparisonExps
  countTypeInput = msCountTypeInput
  aggregateOrderByCountType = MSSQL.IntegerType
  computedField _ _ _ = pure Nothing

instance BackendTableSelectSchema 'MSSQL where
  tableArguments = msTableArgs
  selectTable = defaultSelectTable
  selectTableAggregate = defaultSelectTableAggregate
  tableSelectionSet = defaultTableSelectionSet

instance BackendLogicalModelSelectSchema 'MSSQL where
  logicalModelArguments = defaultLogicalModelArgs
  logicalModelSelectionSet = defaultLogicalModelSelectionSet

instance BackendNativeQuerySelectSchema 'MSSQL where
  selectNativeQuery = NativeQueries.defaultSelectNativeQuery
  selectNativeQueryObject = NativeQueries.defaultSelectNativeQueryObject

instance BackendUpdateOperatorsSchema 'MSSQL where
  type UpdateOperators 'MSSQL = UpdateOperator

  parseUpdateOperators = msParseUpdateOperators

----------------------------------------------------------------

-- * Top level parsers

backendInsertParser ::
  forall m r n.
  (MonadBuildSchema 'MSSQL r m n) =>
  TableInfo 'MSSQL ->
  SchemaT r m (InputFieldsParser n (BackendInsert (UnpreparedValue 'MSSQL)))
backendInsertParser tableInfo = do
  ifMatched <- ifMatchedFieldParser tableInfo
  let _biIdentityColumns = _tciExtraTableMetadata $ _tiCoreInfo tableInfo
  pure $ do
    _biIfMatched <- ifMatched
    pure $ BackendInsert {..}

----------------------------------------------------------------

-- * Table arguments

msTableArgs ::
  forall r m n.
  (MonadBuildSchema 'MSSQL r m n) =>
  TableInfo 'MSSQL ->
  SchemaT r m (InputFieldsParser n (IR.SelectArgsG 'MSSQL (UnpreparedValue 'MSSQL)))
msTableArgs tableInfo = do
  whereParser <- tableWhereArg tableInfo
  orderByParser <- tableOrderByArg tableInfo
  pure do
    whereArg <- whereParser
    orderByArg <- orderByParser
    limitArg <- tableLimitArg
    offsetArg <- tableOffsetArg
    pure
      $ IR.SelectArgs
        { IR._saWhere = whereArg,
          IR._saOrderBy = orderByArg,
          IR._saLimit = limitArg,
          IR._saOffset = offsetArg,
          -- not supported on MSSQL for now
          IR._saDistinct = Nothing
        }

----------------------------------------------------------------

-- * Individual components

msColumnParser ::
  (MonadBuildSchema 'MSSQL r m n) =>
  ColumnType 'MSSQL ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (ValueWithOrigin (ColumnValue 'MSSQL)))
msColumnParser columnType nullability = case columnType of
  -- TODO: the mapping here is not consistent with mkMSSQLScalarTypeName. For
  -- example, exposing all the float types as a GraphQL Float type is
  -- incorrect, similarly exposing all the integer types as a GraphQL Int
  ColumnScalar scalarType ->
    P.memoizeOn 'msColumnParser (scalarType, nullability)
      $ peelWithOrigin
      . fmap (ColumnValue columnType)
      . msPossiblyNullable scalarType nullability
      <$> case scalarType of
        -- text
        MSSQL.CharType -> pure $ mkCharValue <$> P.string
        MSSQL.VarcharType -> pure $ mkCharValue <$> P.string
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
          let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing Nothing [] P.TIScalar
          pure
            $ P.Parser
              { pType = schemaType,
                pParser =
                  P.valueToJSON (P.toGraphQLType schemaType)
                    >=> either (P.parseErrorWith P.ParseFailed . toErrorMessage . qeError) pure
                    . (MSSQL.parseScalarValue scalarType)
              }
  ColumnEnumReference (EnumReference tableName enumValues customTableName) ->
    case nonEmpty (HashMap.toList enumValues) of
      Just enumValuesList ->
        peelWithOrigin
          . fmap (ColumnValue columnType)
          <$> msEnumParser tableName enumValuesList customTableName nullability
      Nothing -> throw400 ValidationFailed "empty enum values"
  where
    -- CHAR/VARCHAR in MSSQL _can_ represent the full UCS (Universal Coded Character Set),
    -- but might not always if the collation used is not UTF-8 enabled
    -- https://docs.microsoft.com/en-us/sql/t-sql/data-types/char-and-varchar-transact-sql?view=sql-server-ver16
    --
    -- NCHAR/NVARCHAR in MSSQL are always able to represent the full UCS
    -- https://docs.microsoft.com/en-us/sql/t-sql/data-types/nchar-and-nvarchar-transact-sql?view=sql-server-ver16
    --
    -- We'd prefer to encode as CHAR/VARCHAR literals to CHAR/VARCHAR columns, as this
    -- means better index performance, BUT as we don't know what the collation
    -- the column is set to (an example is 'SQL_Latin1_General_CP437_BIN') and thus
    -- what characters are available in order to do this safely.
    --
    -- Therefore, we are conservative and only convert on the HGE side when the
    -- characters are all ASCII and guaranteed to be in the target character
    -- set, if not we pass an NCHAR/NVARCHAR and let MSSQL implicitly convert it.

    -- resolves https://github.com/hasura/graphql-engine/issues/8735
    mkCharValue :: Text -> ODBC.Value
    mkCharValue txt =
      if T.all Char.isAscii txt
        then ODBC.ByteStringValue (TE.encodeUtf8 txt) -- an ODBC.ByteStringValue becomes a VARCHAR
        else ODBC.TextValue txt -- an ODBC.TextValue becomes an NVARCHAR

msEnumParser ::
  (MonadBuildSchema 'MSSQL r m n) =>
  TableName 'MSSQL ->
  NonEmpty (EnumValue, EnumValueInfo) ->
  Maybe G.Name ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (ScalarValue 'MSSQL))
msEnumParser tableName enumValues customTableName nullability = do
  enumName <- mkEnumTypeName @'MSSQL tableName customTableName
  pure $ msPossiblyNullable MSSQL.VarcharType nullability $ P.enum enumName Nothing (mkEnumValue <$> enumValues)
  where
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'MSSQL)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) Nothing [] P.EnumValueInfo,
        ODBC.TextValue $ G.unName value
      )

msPossiblyNullable ::
  (MonadParse m) =>
  ScalarType 'MSSQL ->
  G.Nullability ->
  Parser 'Both m (ScalarValue 'MSSQL) ->
  Parser 'Both m (ScalarValue 'MSSQL)
msPossiblyNullable _scalarType (G.Nullability isNullable)
  | isNullable = fmap (fromMaybe ODBC.NullValue) . P.nullable
  | otherwise = id

msOrderByOperators ::
  NamingCase ->
  ( G.Name,
    NonEmpty
      ( P.Definition P.EnumValueInfo,
        (BasicOrderType 'MSSQL, NullsOrderType 'MSSQL)
      )
  )
msOrderByOperators _tCase =
  (Name._order_by,)
    $
    -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
    NE.fromList
      [ ( define Name._asc "in ascending order, nulls first",
          (MSSQL.AscOrder, MSSQL.NullsFirst)
        ),
        ( define Name._asc_nulls_first "in ascending order, nulls first",
          (MSSQL.AscOrder, MSSQL.NullsFirst)
        ),
        ( define Name._asc_nulls_last "in ascending order, nulls last",
          (MSSQL.AscOrder, MSSQL.NullsLast)
        ),
        ( define Name._desc "in descending order, nulls last",
          (MSSQL.DescOrder, MSSQL.NullsLast)
        ),
        ( define Name._desc_nulls_first "in descending order, nulls first",
          (MSSQL.DescOrder, MSSQL.NullsFirst)
        ),
        ( define Name._desc_nulls_last "in descending order, nulls last",
          (MSSQL.DescOrder, MSSQL.NullsLast)
        )
      ]
  where
    define name desc = P.Definition name (Just desc) Nothing [] P.EnumValueInfo

msComparisonExps ::
  forall m n r.
  (MonadBuildSchema 'MSSQL r m n) =>
  ColumnType 'MSSQL ->
  SchemaT r m (Parser 'Input n [ComparisonExp 'MSSQL])
msComparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- retrieve Options.soDangerousBooleanCollapse

  -- parsers used for individual values
  typedParser <- columnParser columnType (G.Nullability False)
  let columnListParser = fmap openValueOrigin <$> P.list typedParser

  -- field info
  let name = P.getName typedParser <> Name.__MSSQL_comparison_exp
      desc =
        G.Description
          $ "Boolean expression to compare columns of type "
          <> P.getName typedParser
          <<> ". All fields are combined with logical 'AND'."

  -- Naming convention
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @'MSSQL

  pure
    $ P.object name (Just desc)
    $ fmap catMaybes
    $ sequenceA
    $ concat
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
                 Name.__like
                 (Just "does the column match the given pattern")
                 (ALIKE . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__nlike
                 (Just "does the column NOT match the given pattern")
                 (ANLIKE . mkParameter <$> typedParser)
             ],
        -- Ops for Geometry/Geography types
        guard (isScalarColumnWhere (`elem` MSSQL.geoTypes) columnType)
          *> [ P.fieldOptional
                 Name.__st_contains
                 (Just "does the column contain the given value")
                 (ABackendSpecific . MSSQL.ASTContains . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__st_equals
                 (Just "is the column equal to given value (directionality is ignored)")
                 (ABackendSpecific . MSSQL.ASTEquals . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__st_intersects
                 (Just "does the column spatially intersect the given value")
                 (ABackendSpecific . MSSQL.ASTIntersects . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__st_overlaps
                 (Just "does the column 'spatially overlap' (intersect but not completely contain) the given value")
                 (ABackendSpecific . MSSQL.ASTOverlaps . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__st_within
                 (Just "is the column contained in the given value")
                 (ABackendSpecific . MSSQL.ASTWithin . mkParameter <$> typedParser)
             ],
        -- Ops for Geometry types
        guard (isScalarColumnWhere (MSSQL.GeometryType ==) columnType)
          *> [ P.fieldOptional
                 Name.__st_crosses
                 (Just "does the column cross the given geometry value")
                 (ABackendSpecific . MSSQL.ASTCrosses . mkParameter <$> typedParser),
               P.fieldOptional
                 Name.__st_touches
                 (Just "does the column have at least one point in common with the given geometry value")
                 (ABackendSpecific . MSSQL.ASTTouches . mkParameter <$> typedParser)
             ]
      ]
  where
    mkListLiteral :: [ColumnValue 'MSSQL] -> UnpreparedValue 'MSSQL
    mkListLiteral =
      UVLiteral . MSSQL.ListExpression . fmap (MSSQL.ValueExpression . cvValue)

msCountTypeInput ::
  (MonadParse n) =>
  Maybe (Parser 'Both n (Column 'MSSQL, AnnRedactionExpUnpreparedValue 'MSSQL)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'MSSQL (UnpreparedValue 'MSSQL))
msCountTypeInput = \case
  Just columnEnum -> do
    column <- P.fieldOptional Name._column Nothing columnEnum
    pure $ flip mkCountType column
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe (Column 'MSSQL, AnnRedactionExpUnpreparedValue 'MSSQL) -> CountType 'MSSQL (UnpreparedValue 'MSSQL)
    mkCountType _ Nothing = MSSQL.CountType MSSQL.StarCountable
    mkCountType IR.SelectCountDistinct (Just (col, redactionExp)) = MSSQL.CountType $ MSSQL.DistinctCountable (col, redactionExp)
    mkCountType IR.SelectCountNonDistinct (Just (col, redactionExp)) = MSSQL.CountType $ MSSQL.NonNullFieldCountable (col, redactionExp)

msParseUpdateOperators ::
  forall m n r.
  (MonadBuildSchema 'MSSQL r m n) =>
  TableInfo 'MSSQL ->
  UpdPermInfo 'MSSQL ->
  SchemaT r m (InputFieldsParser n (HashMap (Column 'MSSQL) (UpdateOperators 'MSSQL (UnpreparedValue 'MSSQL))))
msParseUpdateOperators tableInfo updatePermissions = do
  SU.buildUpdateOperators
    (UpdateSet <$> SU.presetColumns updatePermissions)
    [ UpdateSet <$> SU.setOp,
      UpdateInc <$> SU.incOp
    ]
    tableInfo
