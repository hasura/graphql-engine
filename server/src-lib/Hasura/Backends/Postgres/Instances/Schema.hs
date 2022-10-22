{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Schema
--
-- Defines a 'Hasura.GraphQL.Schema.Backend.BackendSchema' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.Schema
  (
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPathElement (..))
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.Extended qualified as M
import Data.List.NonEmpty qualified as NE
import Data.Parser.JSONPath
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.DML as Postgres hiding (CountType, incOp)
import Hasura.Backends.Postgres.SQL.Types as Postgres hiding (FunctionName, TableName)
import Hasura.Backends.Postgres.SQL.Value as Postgres
import Hasura.Backends.Postgres.Schema.OnConflict
import Hasura.Backends.Postgres.Schema.Select
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.Column
import Hasura.Backends.Postgres.Types.Insert as PGIR
import Hasura.Backends.Postgres.Types.Update as PGIR
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Schema.Backend
  ( BackendSchema,
    BackendTableSelectSchema,
    ComparisonExp,
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Backend qualified as BS
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.BoolExp.AggregationPredicates as Agg
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation qualified as GSB
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser
  ( Definition,
    FieldParser,
    InputFieldsParser,
    Kind (..),
    MonadParse,
    Parser,
    memoize,
    type (<:),
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table (getTableIdentifierName, tableColumns)
import Hasura.GraphQL.Schema.Typename
import Hasura.GraphQL.Schema.Update qualified as SU
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select
  ( QueryDB (QDBConnection),
  )
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Function (FunctionInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table (CustomRootField (..), RolePermInfo (..), TableConfig (..), TableCoreInfoG (..), TableCustomRootFields (..), TableInfo (..), UpdPermInfo (..), ViewInfo (..), getRolePermInfo, isMutable, tableInfoName)
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Citus, Cockroach, Vanilla))
import Hasura.SQL.Tag (HasTag)
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

----------------------------------------------------------------
-- BackendSchema instance

-- | This class is an implementation detail of 'BackendSchema'.
-- Some functions of 'BackendSchema' differ across different Postgres "kinds",
-- or call to functions (such as those related to Relay) that have not been
-- generalized to all kinds of Postgres and still explicitly work on Vanilla
-- Postgres. This class allows each "kind" to specify its own specific
-- implementation. All common code is directly part of `BackendSchema`.
--
-- Note: Users shouldn't ever put this as a constraint. Use `BackendSchema
-- ('Postgres pgKind)` instead.
class PostgresSchema (pgKind :: PostgresKind) where
  pgkBuildTableRelayQueryFields ::
    forall r m n.
    MonadBuildSchema ('Postgres pgKind) r m n =>
    MkRootFieldName ->
    SourceInfo ('Postgres pgKind) ->
    TableName ('Postgres pgKind) ->
    TableInfo ('Postgres pgKind) ->
    C.GQLNameIdentifier ->
    NESeq (ColumnInfo ('Postgres pgKind)) ->
    SchemaT r m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))]
  pgkBuildFunctionRelayQueryFields ::
    forall r m n.
    MonadBuildSchema ('Postgres pgKind) r m n =>
    MkRootFieldName ->
    SourceInfo ('Postgres pgKind) ->
    FunctionName ('Postgres pgKind) ->
    FunctionInfo ('Postgres pgKind) ->
    TableName ('Postgres pgKind) ->
    NESeq (ColumnInfo ('Postgres pgKind)) ->
    SchemaT r m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))]
  pgkRelayExtension ::
    Maybe (XRelay ('Postgres pgKind))

instance PostgresSchema 'Vanilla where
  pgkBuildTableRelayQueryFields = buildTableRelayQueryFields
  pgkBuildFunctionRelayQueryFields = buildFunctionRelayQueryFields
  pgkRelayExtension = Just ()

instance PostgresSchema 'Citus where
  pgkBuildTableRelayQueryFields _ _ _ _ _ _ = pure []
  pgkBuildFunctionRelayQueryFields _ _ _ _ _ _ = pure []
  pgkRelayExtension = Nothing

instance PostgresSchema 'Cockroach where
  pgkBuildTableRelayQueryFields = buildTableRelayQueryFields
  pgkBuildFunctionRelayQueryFields = buildFunctionRelayQueryFields
  pgkRelayExtension = Just ()

-- postgres schema

instance (BackendSchema ('Postgres pgKind)) => AggregationPredicatesSchema ('Postgres pgKind) where
  aggregationPredicatesParser = Agg.defaultAggregationPredicatesParser aggregationFunctions

-- | The aggregation functions that are supported by postgres variants.
aggregationFunctions :: [Agg.FunctionSignature ('Postgres pgKind)]
aggregationFunctions =
  [ Agg.FunctionSignature
      { fnName = "avg",
        fnGQLName = [G.name|avg|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      },
    Agg.FunctionSignature
      { fnName = "bool_and",
        fnGQLName = [G.name|bool_and|],
        fnReturnType = PGBoolean,
        fnArguments = Agg.SingleArgument PGBoolean
      },
    Agg.FunctionSignature
      { fnName = "bool_or",
        fnGQLName = [G.name|bool_or|],
        fnReturnType = PGBoolean,
        fnArguments = Agg.SingleArgument PGBoolean
      },
    Agg.FunctionSignature
      { fnName = "count",
        fnGQLName = [G.name|count|],
        fnReturnType = PGInteger,
        fnArguments = Agg.ArgumentsStar
      },
    Agg.FunctionSignature
      { fnName = "max",
        fnGQLName = [G.name|max|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      },
    Agg.FunctionSignature
      { fnName = "min",
        fnGQLName = [G.name|min|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      },
    Agg.FunctionSignature
      { fnName = "sum",
        fnGQLName = [G.name|sum|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      },
    Agg.FunctionSignature
      { fnName = "corr",
        fnGQLName = [G.name|corr|],
        fnReturnType = PGDouble,
        fnArguments =
          Agg.Arguments
            ( NE.fromList
                [ Agg.ArgumentSignature
                    { argType = PGDouble,
                      argName = [G.name|Y|]
                    },
                  Agg.ArgumentSignature
                    { argType = PGDouble,
                      argName = [G.name|X|]
                    }
                ]
            )
      },
    Agg.FunctionSignature
      { fnName = "covar_samp",
        fnGQLName = [G.name|covar_samp|],
        fnReturnType = PGDouble,
        fnArguments =
          Agg.Arguments
            ( NE.fromList
                [ Agg.ArgumentSignature
                    { argType = PGDouble,
                      argName = [G.name|Y|]
                    },
                  Agg.ArgumentSignature
                    { argType = PGDouble,
                      argName = [G.name|X|]
                    }
                ]
            )
      },
    Agg.FunctionSignature
      { fnName = "stddev_samp",
        fnGQLName = [G.name|stddev_samp|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      },
    Agg.FunctionSignature
      { fnName = "var_samp",
        fnGQLName = [G.name|var_samp|],
        fnReturnType = PGDouble,
        fnArguments = Agg.SingleArgument PGDouble
      }
  ]

instance
  ( PostgresSchema pgKind,
    Backend ('Postgres pgKind),
    HasTag ('Postgres pgKind)
  ) =>
  BS.BackendTableSelectSchema ('Postgres pgKind)
  where
  tableArguments = defaultTableArgs
  selectTable = defaultSelectTable
  selectTableAggregate = defaultSelectTableAggregate
  tableSelectionSet = defaultTableSelectionSet

instance
  ( Backend ('Postgres pgKind),
    PostgresSchema pgKind
  ) =>
  BackendSchema ('Postgres pgKind)
  where
  -- top level parsers
  buildTableQueryAndSubscriptionFields = GSB.buildTableQueryAndSubscriptionFields
  buildTableRelayQueryFields = pgkBuildTableRelayQueryFields
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields = GSB.buildTableInsertMutationFields backendInsertParser
  buildTableUpdateMutationFields = pgkBuildTableUpdateMutationFields
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildFunctionQueryFields = buildFunctionQueryFieldsPG
  buildFunctionRelayQueryFields = pgkBuildFunctionRelayQueryFields
  buildFunctionMutationFields = buildFunctionMutationFieldsPG

  mkRelationshipParser = GSB.mkDefaultRelationshipParser backendInsertParser ()

  -- backend extensions
  relayExtension = pgkRelayExtension @pgKind
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Just ()

  -- indivdual components
  columnParser = columnParser
  enumParser = enumParser @pgKind
  possiblyNullable = possiblyNullable
  scalarSelectionArgumentsParser = pgScalarSelectionArgumentsParser

  -- NOTE: We don't use @orderByOperators@ directly as this will cause memory
  --  growth, instead we use separate functions, according to @jberryman on the
  --  memory growth, "This is turning a CAF Into a function, And the output is
  --  likely no longer going to be shared even for the same arguments, and even
  --  though the domain is extremely small (just HasuraCase or GraphqlCase)."
  orderByOperators _sourceInfo = \case
    HasuraCase -> orderByOperatorsHasuraCase
    GraphqlCase -> orderByOperatorsGraphqlCase
  comparisonExps = const comparisonExps
  countTypeInput = countTypeInput
  aggregateOrderByCountType = Postgres.PGInteger
  computedField = computedFieldPG

backendInsertParser ::
  forall pgKind m r n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  SchemaT r m (InputFieldsParser n (PGIR.BackendInsert pgKind (IR.UnpreparedValue ('Postgres pgKind))))
backendInsertParser sourceName tableInfo =
  fmap BackendInsert <$> onConflictFieldParser sourceName tableInfo

----------------------------------------------------------------
-- Top level parsers

buildTableRelayQueryFields ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  SourceInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  C.GQLNameIdentifier ->
  NESeq (ColumnInfo ('Postgres pgKind)) ->
  SchemaT r m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))]
buildTableRelayQueryFields mkRootFieldName sourceName tableName tableInfo gqlName pkeyColumns = do
  tCase <- asks getter
  let fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
      rootFieldName = runMkRootFieldName mkRootFieldName $ applyFieldNameCaseIdentifier tCase (mkRelayConnectionField gqlName)
  fmap afold $
    optionalFieldParser QDBConnection $
      selectTableConnection sourceName tableInfo rootFieldName fieldDesc pkeyColumns

pgkBuildTableUpdateMutationFields ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  Scenario ->
  -- | The source that the table lives in
  SourceInfo ('Postgres pgKind) ->
  -- | The name of the table being acted on
  TableName ('Postgres pgKind) ->
  -- | table info
  TableInfo ('Postgres pgKind) ->
  -- | field display name
  C.GQLNameIdentifier ->
  SchemaT r m [FieldParser n (IR.AnnotatedUpdateG ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))]
pgkBuildTableUpdateMutationFields mkRootFieldName scenario sourceInfo tableName tableInfo gqlName = do
  -- check in schema options whether we should include multiple updates field
  Options.SchemaOptions {soIncludeUpdateManyFields} <- retrieve id
  roleName <- retrieve scRole
  concat . maybeToList <$> runMaybeT do
    updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
    lift $ do
      -- update_table and update_table_by_pk
      singleUpdates <-
        GSB.buildTableUpdateMutationFields
          -- TODO: https://github.com/hasura/graphql-engine-mono/issues/2955
          (\ti -> fmap BackendUpdate <$> updateOperators ti updatePerms)
          mkRootFieldName
          scenario
          sourceInfo
          tableName
          tableInfo
          gqlName

      -- update_table_many
      multiUpdate <-
        updateTableMany
          mkRootFieldName
          scenario
          sourceInfo
          tableInfo
          gqlName

      -- we only include the multiUpdate field if the
      -- experimental feature 'hide_update_many_fields' is off
      pure $ case soIncludeUpdateManyFields of
        Options.IncludeUpdateManyFields ->
          singleUpdates ++ maybeToList multiUpdate
        Options.DontIncludeUpdateManyFields ->
          singleUpdates

-- | Create a parser for 'update_table_many'. This function is very similar to
-- both 'GSB.buildTableUpdateMutationFields' and
-- 'Hasura.GraphQL.Schema.Update.updateTable'.
--
-- It is similar to the former because of its shape: has to deal with grabbing
-- the casing, deals with update permissions, etc.
--
-- It is similar to the latter because it deals with creating the
-- parser/subselection/etc.
--
-- The reason this function exists here is because it is Postgres specific. It
-- would not fit very well next to the functions mentioned above.
--
-- However, if you are trying to implement this feature for other backends,
-- please consider making this function similar to /updateTable/ and moving it
-- there.
-- Note: this will likely require adding a type or a function to
-- 'BackendSchema'.
updateTableMany ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  Scenario ->
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  C.GQLNameIdentifier ->
  SchemaT r m (Maybe (P.FieldParser n (IR.AnnotatedUpdateG ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))))
updateTableMany mkRootFieldName scenario sourceInfo tableInfo gqlName = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  let columns = tableColumns tableInfo
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsUpdatable viewInfo
  updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
  guard $ not $ scenario == Frontend && upiBackendOnly updatePerms
  updates <- lift (mkMultiRowUpdateParser sourceInfo tableInfo updatePerms)
  selection <- lift $ P.multiple <$> GSB.mutationSelectionSet sourceInfo tableInfo
  let updateName = runMkRootFieldName mkRootFieldName $ GSB.setFieldNameCase tCase tableInfo _tcrfUpdateMany mkUpdateManyField gqlName
      argsParser = liftA2 (,) updates (pure annBoolExpTrue)
  pure $
    P.subselection updateName updateDesc argsParser selection
      <&> SU.mkUpdateObject tableName columns updatePerms (Just tCase) . fmap MOutMultirowFields
  where
    tableName = tableInfoName tableInfo
    defaultUpdateDesc = "update multiples rows of table: " <>> tableName
    updateDesc = GSB.buildFieldDescription defaultUpdateDesc $ _crfComment _tcrfUpdateMany
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

-- | Create a parser for the updates section of the `update_table_many` update.
--
-- It parses a list with two fields: 'where', and an update expression
-- (set/inc/etc).
mkMultiRowUpdateParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  UpdPermInfo ('Postgres pgKind) ->
  SchemaT r m (P.InputFieldsParser n (PGIR.BackendUpdate pgKind (IR.UnpreparedValue ('Postgres pgKind))))
mkMultiRowUpdateParser sourceInfo tableInfo updatePerms = do
  tCase <- asks getter
  tableGQLName <- getTableIdentifierName tableInfo -- getTableGQLName tableInfo
  updatesObjectName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkMultiRowUpdateTypeName tableGQLName
  fmap BackendMultiRowUpdate
    . P.field Name._updates (Just updatesDesc)
    . P.list
    . P.object updatesObjectName Nothing
    <$> do
      mruWhere <- P.field Name._where Nothing <$> boolExp sourceInfo tableInfo
      mruExpression <- updateOperators tableInfo updatePerms
      pure $ MultiRowUpdate <$> mruWhere <*> mruExpression
  where
    updatesDesc = "updates to execute, in order"

buildFunctionRelayQueryFields ::
  forall r m n pgKind.
  ( MonadBuildSchema ('Postgres pgKind) r m n,
    BackendTableSelectSchema ('Postgres pgKind)
  ) =>
  MkRootFieldName ->
  SourceInfo ('Postgres pgKind) ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  NESeq (ColumnInfo ('Postgres pgKind)) ->
  SchemaT r m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue ('Postgres pgKind)))]
buildFunctionRelayQueryFields mkRootFieldName sourceName functionName functionInfo tableName pkeyColumns = do
  let fieldDesc = Just $ G.Description $ "execute function " <> functionName <<> " which returns " <>> tableName
  fmap afold $
    optionalFieldParser QDBConnection $
      selectFunctionConnection mkRootFieldName sourceName functionInfo fieldDesc pkeyColumns

----------------------------------------------------------------
-- Individual components

columnParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  ColumnType ('Postgres pgKind) ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (IR.ValueWithOrigin (ColumnValue ('Postgres pgKind))))
columnParser columnType nullability =
  -- TODO(PDV): It might be worth memoizing this function even though it isn’t
  -- recursive simply for performance reasons, since it’s likely to be hammered
  -- during schema generation. Need to profile to see whether or not it’s a win.
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType ->
      possiblyNullable scalarType nullability <$> do
        -- We convert the value to JSON and use the FromJSON instance. This avoids
        -- having two separate ways of parsing a value in the codebase, which
        -- could lead to inconsistencies.
        --
        -- The mapping from postgres type to GraphQL scalar name is done by
        -- 'mkScalarTypeName'. This is confusing, and we might want to fix it
        -- later, as we will parse values differently here than how they'd be
        -- parsed in other places using the same scalar name; for instance, we
        -- will accept strings for postgres columns of type "Integer", despite the
        -- fact that they will be represented as GraphQL ints, which otherwise do
        -- not accept strings.
        --
        -- TODO: introduce new dedicated scalars for Postgres column types.
        name <- mkScalarTypeName scalarType
        let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing Nothing [] P.TIScalar
        pure $
          P.Parser
            { pType = schemaType,
              pParser =
                P.valueToJSON (P.toGraphQLType schemaType) >=> \case
                  J.Null -> P.parseError $ "unexpected null value for type " <> toErrorValue name
                  value ->
                    runAesonParser (parsePGValue scalarType) value
                      `onLeft` (P.parseErrorWith P.ParseFailed . toErrorMessage . qeError)
            }
    ColumnEnumReference (EnumReference tableName enumValues tableCustomName) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> enumParser @pgKind tableName enumValuesList tableCustomName nullability
        Nothing -> throw400 ValidationFailed "empty enum values"

enumParser ::
  forall pgKind r m n.
  (MonadBuildSchema ('Postgres pgKind) r m n) =>
  TableName ('Postgres pgKind) ->
  NonEmpty (EnumValue, EnumValueInfo) ->
  Maybe G.Name ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (ScalarValue ('Postgres pgKind)))
enumParser tableName enumValues tableCustomName nullability = do
  tCase <- asks getter
  tableGQLName <- liftEither (getIdentifierQualifiedObject tableName)
  name <- addEnumSuffix tableGQLName tableCustomName tCase
  pure $ possiblyNullable PGText nullability $ P.enum name Nothing (mkEnumValue tCase <$> enumValues)
  where
    mkEnumValue :: NamingCase -> (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue ('Postgres pgKind))
    mkEnumValue tCase (EnumValue value, EnumValueInfo description) =
      ( P.Definition (applyEnumValueCase tCase value) (G.Description <$> description) Nothing [] P.EnumValueInfo,
        PGValText $ G.unName value
      )

possiblyNullable ::
  (MonadParse m, 'Input <: k) =>
  ScalarType ('Postgres pgKind) ->
  G.Nullability ->
  Parser k m (ScalarValue ('Postgres pgKind)) ->
  Parser k m (ScalarValue ('Postgres pgKind))
possiblyNullable scalarType (G.Nullability isNullable)
  | isNullable = fmap (fromMaybe $ PGNull scalarType) . P.nullable
  | otherwise = id

pgScalarSelectionArgumentsParser ::
  MonadParse n =>
  ColumnType ('Postgres pgKind) ->
  InputFieldsParser n (Maybe (ScalarSelectionArguments ('Postgres pgKind)))
pgScalarSelectionArgumentsParser columnType
  | isScalarColumnWhere Postgres.isJSONType columnType =
    P.fieldOptional fieldName description P.string `P.bindFields` fmap join . traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = Name._path
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err -> P.parseError $ "parse json path error: " <> toErrorMessage err
      Right [] -> pure Nothing
      Right jPaths -> pure $ Just $ Postgres.ColumnOp Postgres.jsonbPathOp $ Postgres.SEArray $ map elToColExp jPaths
    elToColExp (Key k) = Postgres.SELit $ K.toText k
    elToColExp (Index i) = Postgres.SELit $ tshow i

orderByOperatorsHasuraCase ::
  (G.Name, NonEmpty (Definition P.EnumValueInfo, (BasicOrderType ('Postgres pgKind), NullsOrderType ('Postgres pgKind))))
orderByOperatorsHasuraCase = orderByOperators HasuraCase

orderByOperatorsGraphqlCase ::
  (G.Name, NonEmpty (Definition P.EnumValueInfo, (BasicOrderType ('Postgres pgKind), NullsOrderType ('Postgres pgKind))))
orderByOperatorsGraphqlCase = orderByOperators GraphqlCase

-- | Do NOT use this function directly, this should be used via
--  @orderByOperatorsHasuraCase@ or @orderByOperatorsGraphqlCase@
orderByOperators ::
  NamingCase ->
  (G.Name, NonEmpty (Definition P.EnumValueInfo, (BasicOrderType ('Postgres pgKind), NullsOrderType ('Postgres pgKind))))
orderByOperators tCase =
  (Name._order_by,) $
    NE.fromList
      [ ( define (applyEnumValueCase tCase Name._asc) "in ascending order, nulls last",
          (Postgres.OTAsc, Postgres.NullsLast)
        ),
        ( define (applyEnumValueCase tCase Name._asc_nulls_first) "in ascending order, nulls first",
          (Postgres.OTAsc, Postgres.NullsFirst)
        ),
        ( define (applyEnumValueCase tCase Name._asc_nulls_last) "in ascending order, nulls last",
          (Postgres.OTAsc, Postgres.NullsLast)
        ),
        ( define (applyEnumValueCase tCase Name._desc) "in descending order, nulls first",
          (Postgres.OTDesc, Postgres.NullsFirst)
        ),
        ( define (applyEnumValueCase tCase Name._desc_nulls_first) "in descending order, nulls first",
          (Postgres.OTDesc, Postgres.NullsFirst)
        ),
        ( define (applyEnumValueCase tCase Name._desc_nulls_last) "in descending order, nulls last",
          (Postgres.OTDesc, Postgres.NullsLast)
        )
      ]
  where
    define name desc = P.Definition name (Just desc) Nothing [] P.EnumValueInfo

comparisonExps ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  ColumnType ('Postgres pgKind) ->
  SchemaT r m (Parser 'Input n [ComparisonExp ('Postgres pgKind)])
comparisonExps = memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- retrieve Options.soDangerousBooleanCollapse

  -- parsers used for comparison arguments
  geogInputParser <- geographyWithinDistanceInput
  geomInputParser <- geometryWithinDistanceInput
  ignInputParser <- intersectsGeomNbandInput
  ingInputParser <- intersectsNbandGeomInput
  typedParser <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar PGText) (G.Nullability True)
  textParser <- columnParser (ColumnScalar PGText) (G.Nullability False)
  -- `lquery` represents a regular-expression-like pattern for matching `ltree` values.
  lqueryParser <- columnParser (ColumnScalar PGLquery) (G.Nullability False)
  -- `ltxtquery` represents a full-text-search-like pattern for matching `ltree` values.
  ltxtqueryParser <- columnParser (ColumnScalar PGLtxtquery) (G.Nullability False)
  tCase <- asks getter
  maybeCastParser <- castExp columnType tCase
  let name = applyTypeNameCaseCust tCase $ P.getName typedParser <> Name.__comparison_exp
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      textListParser = fmap IR.openValueOrigin <$> P.list textParser
      columnListParser = fmap IR.openValueOrigin <$> P.list typedParser
  -- Naming conventions
  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ flip (maybe []) maybeCastParser $ \castParser ->
                [ P.fieldOptional Name.__cast Nothing (ACast <$> castParser)
                ],
              -- Common ops for all types
              equalityOperators
                tCase
                collapseIfNull
                (IR.mkParameter <$> typedParser)
                (mkListParameter columnType <$> columnListParser),
              -- Comparison ops for non Raster types
              guard (isScalarColumnWhere (/= PGRaster) columnType)
                *> comparisonOperators
                  tCase
                  collapseIfNull
                  (IR.mkParameter <$> typedParser),
              -- Ops for Raster types
              guard (isScalarColumnWhere (== PGRaster) columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects", "rast"]))
                       Nothing
                       (ABackendSpecific . ASTIntersectsRast . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects", "nband", "geom"]))
                       Nothing
                       (ABackendSpecific . ASTIntersectsNbandGeom <$> ingInputParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects", "geom", "nband"]))
                       Nothing
                       (ABackendSpecific . ASTIntersectsGeomNband <$> ignInputParser)
                   ],
              -- Ops for String like types
              guard (isScalarColumnWhere isStringType columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__like)
                       (Just "does the column match the given pattern")
                       (ALIKE . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__nlike)
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__ilike)
                       (Just "does the column match the given case-insensitive pattern")
                       (ABackendSpecific . AILIKE . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__nilike)
                       (Just "does the column NOT match the given case-insensitive pattern")
                       (ABackendSpecific . ANILIKE . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__similar)
                       (Just "does the column match the given SQL regular expression")
                       (ABackendSpecific . ASIMILAR . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__nsimilar)
                       (Just "does the column NOT match the given SQL regular expression")
                       (ABackendSpecific . ANSIMILAR . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__regex)
                       (Just "does the column match the given POSIX regular expression, case sensitive")
                       (ABackendSpecific . AREGEX . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__iregex)
                       (Just "does the column match the given POSIX regular expression, case insensitive")
                       (ABackendSpecific . AIREGEX . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__nregex)
                       (Just "does the column NOT match the given POSIX regular expression, case sensitive")
                       (ABackendSpecific . ANREGEX . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__niregex)
                       (Just "does the column NOT match the given POSIX regular expression, case insensitive")
                       (ABackendSpecific . ANIREGEX . IR.mkParameter <$> typedParser)
                   ],
              -- Ops for JSONB type
              guard (isScalarColumnWhere (== PGJSONB) columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__contains)
                       (Just "does the column contain the given json value at the top level")
                       (ABackendSpecific . AContains . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_contained", "in"]))
                       (Just "is the column contained in the given json value")
                       (ABackendSpecific . AContainedIn . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_has", "key"]))
                       (Just "does the string exist as a top-level key in the column")
                       (ABackendSpecific . AHasKey . IR.mkParameter <$> nullableTextParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_has", "keys", "any"]))
                       (Just "do any of these strings exist as top-level keys in the column")
                       (ABackendSpecific . AHasKeysAny . mkListLiteral (ColumnScalar PGText) <$> textListParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_has", "keys", "all"]))
                       (Just "do all of these strings exist as top-level keys in the column")
                       (ABackendSpecific . AHasKeysAll . mkListLiteral (ColumnScalar PGText) <$> textListParser)
                   ],
              -- Ops for Geography type
              guard (isScalarColumnWhere (== PGGeography) columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects"]))
                       (Just "does the column spatially intersect the given geography value")
                       (ABackendSpecific . ASTIntersects . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "d", "within"]))
                       (Just "is the column within a given distance from the given geography value")
                       (ABackendSpecific . ASTDWithinGeog <$> geogInputParser)
                   ],
              -- Ops for Geometry type
              guard (isScalarColumnWhere (== PGGeometry) columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "contains"]))
                       (Just "does the column contain the given geometry value")
                       (ABackendSpecific . ASTContains . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "crosses"]))
                       (Just "does the column cross the given geometry value")
                       (ABackendSpecific . ASTCrosses . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "equals"]))
                       (Just "is the column equal to given geometry value (directionality is ignored)")
                       (ABackendSpecific . ASTEquals . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "overlaps"]))
                       (Just "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value")
                       (ABackendSpecific . ASTOverlaps . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "touches"]))
                       (Just "does the column have atleast one point in common with the given geometry value")
                       (ABackendSpecific . ASTTouches . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "within"]))
                       (Just "is the column contained in the given geometry value")
                       (ABackendSpecific . ASTWithin . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects"]))
                       (Just "does the column spatially intersect the given geometry value")
                       (ABackendSpecific . ASTIntersects . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "3d", "intersects"]))
                       (Just "does the column spatially intersect the given geometry value in 3D")
                       (ABackendSpecific . AST3DIntersects . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "d", "within"]))
                       (Just "is the column within a given distance from the given geometry value")
                       (ABackendSpecific . ASTDWithinGeom <$> geomInputParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "3d", "d", "within"]))
                       (Just "is the column within a given 3D distance from the given geometry value")
                       (ABackendSpecific . AST3DDWithinGeom <$> geomInputParser)
                   ],
              -- Ops for Ltree type
              guard (isScalarColumnWhere (== PGLtree) columnType)
                *> [ mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__ancestor)
                       (Just "is the left argument an ancestor of right (or equal)?")
                       (ABackendSpecific . AAncestor . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_ancestor", "any"]))
                       (Just "does array contain an ancestor of `ltree`?")
                       (ABackendSpecific . AAncestorAny . mkListLiteral columnType <$> columnListParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__descendant)
                       (Just "is the left argument a descendant of right (or equal)?")
                       (ABackendSpecific . ADescendant . IR.mkParameter <$> typedParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_descendant", "any"]))
                       (Just "does array contain a descendant of `ltree`?")
                       (ABackendSpecific . ADescendantAny . mkListLiteral columnType <$> columnListParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedName Name.__matches)
                       (Just "does `ltree` match `lquery`?")
                       (ABackendSpecific . AMatches . IR.mkParameter <$> lqueryParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_matches", "any"]))
                       (Just "does `ltree` match any `lquery` in array?")
                       (ABackendSpecific . AMatchesAny . mkListLiteral (ColumnScalar PGLquery) <$> textListParser),
                     mkBoolOperator
                       tCase
                       collapseIfNull
                       (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_matches", "fulltext"]))
                       (Just "does `ltree` match `ltxtquery`?")
                       (ABackendSpecific . AMatchesFulltext . IR.mkParameter <$> ltxtqueryParser)
                   ]
            ]
  where
    mkListLiteral :: ColumnType ('Postgres pgKind) -> [ColumnValue ('Postgres pgKind)] -> IR.UnpreparedValue ('Postgres pgKind)
    mkListLiteral columnType columnValues =
      IR.UVLiteral $
        SETyAnn
          (SEArray $ txtEncoder . cvValue <$> columnValues)
          (mkTypeAnn $ CollectableTypeArray $ unsafePGColumnToBackend columnType)
    mkListParameter :: ColumnType ('Postgres pgKind) -> [ColumnValue ('Postgres pgKind)] -> IR.UnpreparedValue ('Postgres pgKind)
    mkListParameter columnType columnValues = do
      let scalarType = unsafePGColumnToBackend columnType
      IR.UVParameter Nothing $
        ColumnValue
          (ColumnScalar $ Postgres.PGArray scalarType)
          (Postgres.PGValArray $ cvValue <$> columnValues)

    castExp :: ColumnType ('Postgres pgKind) -> NamingCase -> SchemaT r m (Maybe (Parser 'Input n (CastExp ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind)))))
    castExp sourceType tCase = do
      let maybeScalars = case sourceType of
            ColumnScalar PGGeography -> Just (PGGeography, PGGeometry)
            ColumnScalar PGGeometry -> Just (PGGeometry, PGGeography)
            ColumnScalar PGJSONB -> Just (PGJSONB, PGText)
            _ -> Nothing

      forM maybeScalars $ \(sourceScalar, targetScalar) -> do
        scalarTypeName <- C.fromAutogeneratedName <$> mkScalarTypeName sourceScalar
        targetName <- mkScalarTypeName targetScalar
        targetOpExps <- comparisonExps $ ColumnScalar targetScalar
        let field = P.fieldOptional targetName Nothing $ (targetScalar,) <$> targetOpExps
            sourceName = applyTypeNameCaseIdentifier tCase (scalarTypeName <> (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["cast", "exp"])))
        pure $ P.object sourceName Nothing $ M.fromList . maybeToList <$> field

geographyWithinDistanceInput ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SchemaT r m (Parser 'Input n (DWithinGeogOp (IR.UnpreparedValue ('Postgres pgKind))))
geographyWithinDistanceInput = do
  geographyParser <- columnParser (ColumnScalar PGGeography) (G.Nullability False)
  -- FIXME
  -- It doesn't make sense for this value to be nullable; it only is for
  -- backwards compatibility; if an explicit Null value is given, it will be
  -- forwarded to the underlying SQL function, that in turns treat a null value
  -- as an error. We can fix this by rejecting explicit null values, by marking
  -- this field non-nullable in a future release.
  booleanParser <- columnParser (ColumnScalar PGBoolean) (G.Nullability True)
  floatParser <- columnParser (ColumnScalar PGFloat) (G.Nullability False)
  pure $
    P.object Name._st_d_within_geography_input Nothing $
      DWithinGeogOp <$> (IR.mkParameter <$> P.field Name._distance Nothing floatParser)
        <*> (IR.mkParameter <$> P.field Name._from Nothing geographyParser)
        <*> (IR.mkParameter <$> P.fieldWithDefault Name._use_spheroid Nothing (G.VBoolean True) booleanParser)

geometryWithinDistanceInput ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SchemaT r m (Parser 'Input n (DWithinGeomOp (IR.UnpreparedValue ('Postgres pgKind))))
geometryWithinDistanceInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  floatParser <- columnParser (ColumnScalar PGFloat) (G.Nullability False)
  pure $
    P.object Name._st_d_within_input Nothing $
      DWithinGeomOp <$> (IR.mkParameter <$> P.field Name._distance Nothing floatParser)
        <*> (IR.mkParameter <$> P.field Name._from Nothing geometryParser)

intersectsNbandGeomInput ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SchemaT r m (Parser 'Input n (STIntersectsNbandGeommin (IR.UnpreparedValue ('Postgres pgKind))))
intersectsNbandGeomInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser <- columnParser (ColumnScalar PGInteger) (G.Nullability False)
  pure $
    P.object Name._st_intersects_nband_geom_input Nothing $
      STIntersectsNbandGeommin <$> (IR.mkParameter <$> P.field Name._nband Nothing integerParser)
        <*> (IR.mkParameter <$> P.field Name._geommin Nothing geometryParser)

intersectsGeomNbandInput ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SchemaT r m (Parser 'Input n (STIntersectsGeomminNband (IR.UnpreparedValue ('Postgres pgKind))))
intersectsGeomNbandInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser <- columnParser (ColumnScalar PGInteger) (G.Nullability False)
  pure $
    P.object Name._st_intersects_geom_nband_input Nothing $
      STIntersectsGeomminNband
        <$> (IR.mkParameter <$> P.field Name._geommin Nothing geometryParser)
        <*> (fmap IR.mkParameter <$> P.fieldOptional Name._nband Nothing integerParser)

countTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column ('Postgres pgKind))) ->
  InputFieldsParser n (IR.CountDistinct -> CountType ('Postgres pgKind))
countTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional Name._columns Nothing (P.list columnEnum)
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [Column ('Postgres pgKind)] -> CountType ('Postgres pgKind)
    mkCountType _ Nothing = Postgres.CTStar
    mkCountType IR.SelectCountDistinct (Just cols) = Postgres.CTDistinct cols
    mkCountType IR.SelectCountNonDistinct (Just cols) = Postgres.CTSimple cols

-- | Update operator that prepends a value to a column containing jsonb arrays.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
prependOp ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SU.UpdateOperator ('Postgres pgKind) r m n (IR.UnpreparedValue ('Postgres pgKind))
prependOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName _tableName columns = do
      let typedParser columnInfo =
            fmap IR.mkParameter
              <$> BS.columnParser
                (ciType columnInfo)
                (G.Nullability $ ciIsNullable columnInfo)

          desc = "prepend existing jsonb value of filtered columns with new jsonb value"

      SU.updateOperator
        tableGQLName
        (C.fromAutogeneratedName $$(G.litName "prepend"))
        (C.fromAutogeneratedName $$(G.litName "_prepend"))
        typedParser
        columns
        desc
        desc

-- | Update operator that appends a value to a column containing jsonb arrays.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
appendOp ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SU.UpdateOperator ('Postgres pgKind) r m n (IR.UnpreparedValue ('Postgres pgKind))
appendOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName _tableName columns = do
      let typedParser columnInfo =
            fmap IR.mkParameter
              <$> BS.columnParser
                (ciType columnInfo)
                (G.Nullability $ ciIsNullable columnInfo)

          desc = "append existing jsonb value of filtered columns with new jsonb value"
      SU.updateOperator
        tableGQLName
        (C.fromAutogeneratedName $$(G.litName "append"))
        (C.fromAutogeneratedName $$(G.litName "_append"))
        typedParser
        columns
        desc
        desc

-- | Update operator that deletes a value at a specified key from a column
-- containing jsonb objects.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
deleteKeyOp ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SU.UpdateOperator ('Postgres pgKind) r m n (IR.UnpreparedValue ('Postgres pgKind))
deleteKeyOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName _tableName columns = do
      let nullableTextParser _ = fmap IR.mkParameter <$> BS.columnParser (ColumnScalar PGText) (G.Nullability True)
          desc = "delete key/value pair or string element. key/value pairs are matched based on their key value"

      SU.updateOperator
        tableGQLName
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["delete", "key"]))
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_delete", "key"]))
        nullableTextParser
        columns
        desc
        desc

-- | Update operator that deletes a value at a specific index from a column
-- containing jsonb arrays.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
deleteElemOp ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SU.UpdateOperator ('Postgres pgKind) r m n (IR.UnpreparedValue ('Postgres pgKind))
deleteElemOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName _tableName columns = do
      let nonNullableIntParser _ = fmap IR.mkParameter <$> BS.columnParser (ColumnScalar PGInteger) (G.Nullability False)
          desc =
            "delete the array element with specified index (negative integers count from the end). "
              <> "throws an error if top level container is not an array"

      SU.updateOperator
        tableGQLName
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["delete", "elem"]))
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_delete", "elem"]))
        nonNullableIntParser
        columns
        desc
        desc

-- | Update operator that deletes a field at a certan path from a column
-- containing jsonb objects.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
deleteAtPathOp ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SU.UpdateOperator ('Postgres pgKind) r m n [IR.UnpreparedValue ('Postgres pgKind)]
deleteAtPathOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName _tableName columns = do
      let nonNullableTextListParser _ = P.list . fmap IR.mkParameter <$> BS.columnParser (ColumnScalar PGText) (G.Nullability False)
          desc = "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"

      SU.updateOperator
        tableGQLName
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["delete", "at", "path"]))
        (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_delete", "at", "path"]))
        nonNullableTextListParser
        columns
        desc
        desc

-- | The update operators that we support on Postgres.
updateOperators ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  TableInfo ('Postgres pgKind) ->
  UpdPermInfo ('Postgres pgKind) ->
  SchemaT r m (InputFieldsParser n (HashMap (Column ('Postgres pgKind)) (UpdateOpExpression (IR.UnpreparedValue ('Postgres pgKind)))))
updateOperators tableInfo updatePermissions = do
  SU.buildUpdateOperators
    (PGIR.UpdateSet <$> SU.presetColumns updatePermissions)
    [ PGIR.UpdateSet <$> SU.setOp,
      PGIR.UpdateInc <$> SU.incOp,
      PGIR.UpdatePrepend <$> prependOp,
      PGIR.UpdateAppend <$> appendOp,
      PGIR.UpdateDeleteKey <$> deleteKeyOp,
      PGIR.UpdateDeleteElem <$> deleteElemOp,
      PGIR.UpdateDeleteAtPath <$> deleteAtPathOp
    ]
    tableInfo
