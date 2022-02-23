{-# LANGUAGE ApplicativeDo #-}
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
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.Extended qualified as M
import Data.List.NonEmpty qualified as NE
import Data.Parser.JSONPath
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.DML as PG hiding (CountType, incOp)
import Hasura.Backends.Postgres.SQL.Types as PG hiding (FunctionName, TableName)
import Hasura.Backends.Postgres.SQL.Value as PG
import Hasura.Backends.Postgres.Schema.OnConflict
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.Column
import Hasura.Backends.Postgres.Types.Insert as PGIR
import Hasura.Backends.Postgres.Types.Update as PGIR
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import Hasura.GraphQL.Schema.Backend
  ( BackendSchema,
    ComparisonExp,
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Backend qualified as BS
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation qualified as GSB
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Update qualified as SU
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select
  ( QueryDB (QDBConnection),
  )
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function (FunctionInfo)
import Hasura.RQL.Types.SourceCustomization (mkRootFieldName)
import Hasura.RQL.Types.Table (RolePermInfo (..), SelPermInfo, TableInfo, UpdPermInfo)
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Citus, Vanilla))
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

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
    BS.MonadBuildSchema ('Postgres pgKind) r m n =>
    SourceName ->
    TableName ('Postgres pgKind) ->
    TableInfo ('Postgres pgKind) ->
    G.Name ->
    NESeq (ColumnInfo ('Postgres pgKind)) ->
    m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
  pgkBuildFunctionRelayQueryFields ::
    BS.MonadBuildSchema ('Postgres pgKind) r m n =>
    SourceName ->
    FunctionName ('Postgres pgKind) ->
    FunctionInfo ('Postgres pgKind) ->
    TableName ('Postgres pgKind) ->
    NESeq (ColumnInfo ('Postgres pgKind)) ->
    m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
  pgkRelayExtension ::
    Maybe (XRelay ('Postgres pgKind))
  pgkNode ::
    BS.MonadBuildSchema ('Postgres pgKind) r m n =>
    m
      ( Parser
          'Output
          n
          ( HashMap
              ( TableName ('Postgres pgKind)
              )
              ( SourceName,
                SourceConfig ('Postgres pgKind),
                SelPermInfo ('Postgres pgKind),
                PrimaryKeyColumns ('Postgres pgKind),
                AnnotatedFields ('Postgres pgKind)
              )
          )
      )

instance PostgresSchema 'Vanilla where
  pgkBuildTableRelayQueryFields = buildTableRelayQueryFields
  pgkBuildFunctionRelayQueryFields = buildFunctionRelayQueryFields
  pgkRelayExtension = Just ()
  pgkNode = nodePG

instance PostgresSchema 'Citus where
  pgkBuildTableRelayQueryFields _ _ _ _ _ = pure []
  pgkBuildFunctionRelayQueryFields _ _ _ _ _ = pure []
  pgkRelayExtension = Nothing
  pgkNode = undefined

-- postgres schema

instance
  ( Backend ('Postgres pgKind),
    PostgresSchema pgKind
  ) =>
  BackendSchema ('Postgres pgKind)
  where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields
  buildTableRelayQueryFields = pgkBuildTableRelayQueryFields
  buildTableInsertMutationFields = GSB.buildTableInsertMutationFields backendInsertParser
  buildTableUpdateMutationFields = pgkBuildTableUpdateMutationFields
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildFunctionQueryFields = GSB.buildFunctionQueryFields
  buildFunctionRelayQueryFields = pgkBuildFunctionRelayQueryFields
  buildFunctionMutationFields = GSB.buildFunctionMutationFields

  -- table components
  tableArguments = defaultTableArgs
  mkRelationshipParser = GSB.mkDefaultRelationshipParser backendInsertParser ()

  -- backend extensions
  relayExtension = pgkRelayExtension @pgKind
  nodesAggExtension = Just ()

  -- indivdual components
  columnParser = columnParser
  jsonPathArg = jsonPathArg
  orderByOperators = orderByOperators
  comparisonExps = comparisonExps
  countTypeInput = countTypeInput
  aggregateOrderByCountType = PG.PGInteger
  computedField = computedFieldPG
  node = pgkNode

  -- SQL literals
  columnDefaultValue = const PG.columnDefaultValue

backendInsertParser ::
  forall pgKind m r n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  TableInfo ('Postgres pgKind) ->
  m (InputFieldsParser n (PGIR.BackendInsert pgKind (UnpreparedValue ('Postgres pgKind))))
backendInsertParser sourceName tableInfo =
  fmap BackendInsert <$> onConflictFieldParser sourceName tableInfo

----------------------------------------------------------------
-- Top level parsers

buildTableRelayQueryFields ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  TableName ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  G.Name ->
  NESeq (ColumnInfo ('Postgres pgKind)) ->
  m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildTableRelayQueryFields sourceName tableName tableInfo gqlName pkeyColumns = do
  let fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
  fieldName <- mkRootFieldName $ gqlName <> $$(G.litName "_connection")
  fmap afold $
    optionalFieldParser QDBConnection $
      selectTableConnection sourceName tableInfo fieldName fieldDesc pkeyColumns

pgkBuildTableUpdateMutationFields ::
  MonadBuildSchema ('Postgres pgKind) r m n =>
  -- | The source that the table lives in
  SourceName ->
  -- | The name of the table being acted on
  TableName ('Postgres pgKind) ->
  -- | table info
  TableInfo ('Postgres pgKind) ->
  -- | field display name
  G.Name ->
  m [FieldParser n (IR.AnnotatedUpdateG ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
pgkBuildTableUpdateMutationFields sourceName tableName tableInfo gqlName =
  concat . maybeToList <$> runMaybeT do
    updatePerms <- MaybeT $ (_permUpd =<<) <$> tablePermissions tableInfo
    lift $
      GSB.buildTableUpdateMutationFields
        -- TODO: https://github.com/hasura/graphql-engine-mono/issues/2955
        (\ti -> fmap BackendUpdate <$> updateOperators ti updatePerms)
        sourceName
        tableName
        tableInfo
        gqlName

buildFunctionRelayQueryFields ::
  forall pgKind m n r.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  NESeq (ColumnInfo ('Postgres pgKind)) ->
  m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionRelayQueryFields sourceName functionName functionInfo tableName pkeyColumns = do
  let fieldDesc = Just $ G.Description $ "execute function " <> functionName <<> " which returns " <>> tableName
  fmap afold $
    optionalFieldParser QDBConnection $
      selectFunctionConnection sourceName functionInfo fieldDesc pkeyColumns

----------------------------------------------------------------
-- Individual components

columnParser ::
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has P.MkTypename r) =>
  ColumnType ('Postgres pgKind) ->
  G.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (ColumnValue ('Postgres pgKind))))
columnParser columnType (G.Nullability isNullable) =
  -- TODO(PDV): It might be worth memoizing this function even though it isn’t
  -- recursive simply for performance reasons, since it’s likely to be hammered
  -- during schema generation. Need to profile to see whether or not it’s a win.
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType ->
      possiblyNullable scalarType <$> do
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
        let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing P.TIScalar
        pure $
          Parser
            { pType = schemaType,
              pParser =
                valueToJSON (P.toGraphQLType schemaType) >=> \case
                  J.Null -> parseError $ "unexpected null value for type " <>> name
                  value ->
                    runAesonParser (parsePGValue scalarType) value
                      `onLeft` (parseErrorWith ParseFailed . qeError)
            }
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          name <- qualifiedObjectToName tableName <&> (<> $$(G.litName "_enum")) >>= P.mkTypename
          pure $ possiblyNullable PGText $ P.enum name Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable scalarType
      | isNullable = fmap (fromMaybe $ PGNull scalarType) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, PGScalarValue)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) P.EnumValueInfo,
        PGValText $ G.unName value
      )

jsonPathArg ::
  MonadParse n =>
  ColumnType ('Postgres pgKind) ->
  InputFieldsParser n (Maybe (IR.ColumnOp ('Postgres pgKind)))
jsonPathArg columnType
  | isScalarColumnWhere PG.isJSONType columnType =
    P.fieldOptional fieldName description P.string `P.bindFields` fmap join . traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = $$(G.litName "path")
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err -> parseError $ T.pack $ "parse json path error: " ++ err
      Right [] -> pure Nothing
      Right jPaths -> pure $ Just $ IR.ColumnOp PG.jsonbPathOp $ PG.SEArray $ map elToColExp jPaths
    elToColExp (Key k) = PG.SELit k
    elToColExp (Index i) = PG.SELit $ tshow i

orderByOperators ::
  NonEmpty (Definition P.EnumValueInfo, (BasicOrderType ('Postgres pgKind), NullsOrderType ('Postgres pgKind)))
orderByOperators =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls last",
        (PG.OTAsc, PG.NLast)
      ),
      ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first",
        (PG.OTAsc, PG.NFirst)
      ),
      ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last",
        (PG.OTAsc, PG.NLast)
      ),
      ( define $$(G.litName "desc") "in descending order, nulls first",
        (PG.OTDesc, PG.NFirst)
      ),
      ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first",
        (PG.OTDesc, PG.NFirst)
      ),
      ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last",
        (PG.OTDesc, PG.NLast)
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

comparisonExps ::
  forall pgKind m n r.
  ( BackendSchema ('Postgres pgKind),
    MonadSchema n m,
    MonadError QErr m,
    MonadReader r m,
    Has QueryContext r,
    Has MkTypename r
  ) =>
  ColumnType ('Postgres pgKind) ->
  m (Parser 'Input n [ComparisonExp ('Postgres pgKind)])
comparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- asks $ qcDangerousBooleanCollapse . getter

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
  maybeCastParser <- castExp columnType
  let name = P.getName typedParser <> $$(G.litName "_comparison_exp")
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      textListParser = fmap openValueOrigin <$> P.list textParser
      columnListParser = fmap openValueOrigin <$> P.list typedParser

  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ flip (maybe []) maybeCastParser $ \castParser ->
                [ P.fieldOptional $$(G.litName "_cast") Nothing (ACast <$> castParser)
                ],
              -- Common ops for all types
              equalityOperators
                collapseIfNull
                (mkParameter <$> typedParser)
                (mkListLiteral columnType <$> columnListParser),
              -- Comparison ops for non Raster types
              guard (isScalarColumnWhere (/= PGRaster) columnType)
                *> comparisonOperators
                  collapseIfNull
                  (mkParameter <$> typedParser),
              -- Ops for Raster types
              guard (isScalarColumnWhere (== PGRaster) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects_rast")
                       Nothing
                       (ABackendSpecific . ASTIntersectsRast . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects_nband_geom")
                       Nothing
                       (ABackendSpecific . ASTIntersectsNbandGeom <$> ingInputParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects_geom_nband")
                       Nothing
                       (ABackendSpecific . ASTIntersectsGeomNband <$> ignInputParser)
                   ],
              -- Ops for String like types
              guard (isScalarColumnWhere isStringType columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_like")
                       (Just "does the column match the given pattern")
                       (ALIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nlike")
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_ilike")
                       (Just "does the column match the given case-insensitive pattern")
                       (ABackendSpecific . AILIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nilike")
                       (Just "does the column NOT match the given case-insensitive pattern")
                       (ABackendSpecific . ANILIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_similar")
                       (Just "does the column match the given SQL regular expression")
                       (ABackendSpecific . ASIMILAR . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nsimilar")
                       (Just "does the column NOT match the given SQL regular expression")
                       (ABackendSpecific . ANSIMILAR . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_regex")
                       (Just "does the column match the given POSIX regular expression, case sensitive")
                       (ABackendSpecific . AREGEX . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_iregex")
                       (Just "does the column match the given POSIX regular expression, case insensitive")
                       (ABackendSpecific . AIREGEX . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nregex")
                       (Just "does the column NOT match the given POSIX regular expression, case sensitive")
                       (ABackendSpecific . ANREGEX . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_niregex")
                       (Just "does the column NOT match the given POSIX regular expression, case insensitive")
                       (ABackendSpecific . ANIREGEX . mkParameter <$> typedParser)
                   ],
              -- Ops for JSONB type
              guard (isScalarColumnWhere (== PGJSONB) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_contains")
                       (Just "does the column contain the given json value at the top level")
                       (ABackendSpecific . AContains . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_contained_in")
                       (Just "is the column contained in the given json value")
                       (ABackendSpecific . AContainedIn . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_has_key")
                       (Just "does the string exist as a top-level key in the column")
                       (ABackendSpecific . AHasKey . mkParameter <$> nullableTextParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_has_keys_any")
                       (Just "do any of these strings exist as top-level keys in the column")
                       (ABackendSpecific . AHasKeysAny . mkListLiteral (ColumnScalar PGText) <$> textListParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_has_keys_all")
                       (Just "do all of these strings exist as top-level keys in the column")
                       (ABackendSpecific . AHasKeysAll . mkListLiteral (ColumnScalar PGText) <$> textListParser)
                   ],
              -- Ops for Geography type
              guard (isScalarColumnWhere (== PGGeography) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects")
                       (Just "does the column spatially intersect the given geography value")
                       (ABackendSpecific . ASTIntersects . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_d_within")
                       (Just "is the column within a given distance from the given geography value")
                       (ABackendSpecific . ASTDWithinGeog <$> geogInputParser)
                   ],
              -- Ops for Geometry type
              guard (isScalarColumnWhere (== PGGeometry) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_contains")
                       (Just "does the column contain the given geometry value")
                       (ABackendSpecific . ASTContains . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_crosses")
                       (Just "does the column cross the given geometry value")
                       (ABackendSpecific . ASTCrosses . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_equals")
                       (Just "is the column equal to given geometry value (directionality is ignored)")
                       (ABackendSpecific . ASTEquals . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_overlaps")
                       (Just "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value")
                       (ABackendSpecific . ASTOverlaps . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_touches")
                       (Just "does the column have atleast one point in common with the given geometry value")
                       (ABackendSpecific . ASTTouches . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_within")
                       (Just "is the column contained in the given geometry value")
                       (ABackendSpecific . ASTWithin . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects")
                       (Just "does the column spatially intersect the given geometry value")
                       (ABackendSpecific . ASTIntersects . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_3d_intersects")
                       (Just "does the column spatially intersect the given geometry value in 3D")
                       (ABackendSpecific . AST3DIntersects . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_d_within")
                       (Just "is the column within a given distance from the given geometry value")
                       (ABackendSpecific . ASTDWithinGeom <$> geomInputParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_3d_d_within")
                       (Just "is the column within a given 3D distance from the given geometry value")
                       (ABackendSpecific . AST3DDWithinGeom <$> geomInputParser)
                   ],
              -- Ops for Ltree type
              guard (isScalarColumnWhere (== PGLtree) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_ancestor")
                       (Just "is the left argument an ancestor of right (or equal)?")
                       (ABackendSpecific . AAncestor . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_ancestor_any")
                       (Just "does array contain an ancestor of `ltree`?")
                       (ABackendSpecific . AAncestorAny . mkListLiteral columnType <$> columnListParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_descendant")
                       (Just "is the left argument a descendant of right (or equal)?")
                       (ABackendSpecific . ADescendant . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_descendant_any")
                       (Just "does array contain a descendant of `ltree`?")
                       (ABackendSpecific . ADescendantAny . mkListLiteral columnType <$> columnListParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_matches")
                       (Just "does `ltree` match `lquery`?")
                       (ABackendSpecific . AMatches . mkParameter <$> lqueryParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_matches_any")
                       (Just "does `ltree` match any `lquery` in array?")
                       (ABackendSpecific . AMatchesAny . mkListLiteral (ColumnScalar PGLquery) <$> textListParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_matches_fulltext")
                       (Just "does `ltree` match `ltxtquery`?")
                       (ABackendSpecific . AMatchesFulltext . mkParameter <$> ltxtqueryParser)
                   ]
            ]
  where
    mkListLiteral :: ColumnType ('Postgres pgKind) -> [ColumnValue ('Postgres pgKind)] -> UnpreparedValue ('Postgres pgKind)
    mkListLiteral columnType columnValues =
      P.UVLiteral $
        SETyAnn
          (SEArray $ txtEncoder . cvValue <$> columnValues)
          (mkTypeAnn $ CollectableTypeArray $ unsafePGColumnToBackend columnType)

    castExp :: ColumnType ('Postgres pgKind) -> m (Maybe (Parser 'Input n (CastExp ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind)))))
    castExp sourceType = do
      let maybeScalars = case sourceType of
            ColumnScalar PGGeography -> Just (PGGeography, PGGeometry)
            ColumnScalar PGGeometry -> Just (PGGeometry, PGGeography)
            _ -> Nothing

      forM maybeScalars $ \(sourceScalar, targetScalar) -> do
        sourceName <- mkScalarTypeName sourceScalar <&> (<> $$(G.litName "_cast_exp"))
        targetName <- mkScalarTypeName targetScalar
        targetOpExps <- comparisonExps $ ColumnScalar targetScalar
        let field = P.fieldOptional targetName Nothing $ (targetScalar,) <$> targetOpExps
        pure $ P.object sourceName Nothing $ M.fromList . maybeToList <$> field

geographyWithinDistanceInput ::
  forall pgKind m n r.
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  m (Parser 'Input n (DWithinGeogOp (UnpreparedValue ('Postgres pgKind))))
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
    P.object $$(G.litName "st_d_within_geography_input") Nothing $
      DWithinGeogOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
        <*> (mkParameter <$> P.field $$(G.litName "from") Nothing geographyParser)
        <*> (mkParameter <$> P.fieldWithDefault $$(G.litName "use_spheroid") Nothing (G.VBoolean True) booleanParser)

geometryWithinDistanceInput ::
  forall pgKind m n r.
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  m (Parser 'Input n (DWithinGeomOp (UnpreparedValue ('Postgres pgKind))))
geometryWithinDistanceInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  floatParser <- columnParser (ColumnScalar PGFloat) (G.Nullability False)
  pure $
    P.object $$(G.litName "st_d_within_input") Nothing $
      DWithinGeomOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
        <*> (mkParameter <$> P.field $$(G.litName "from") Nothing geometryParser)

intersectsNbandGeomInput ::
  forall pgKind m n r.
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  m (Parser 'Input n (STIntersectsNbandGeommin (UnpreparedValue ('Postgres pgKind))))
intersectsNbandGeomInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser <- columnParser (ColumnScalar PGInteger) (G.Nullability False)
  pure $
    P.object $$(G.litName "st_intersects_nband_geom_input") Nothing $
      STIntersectsNbandGeommin <$> (mkParameter <$> P.field $$(G.litName "nband") Nothing integerParser)
        <*> (mkParameter <$> P.field $$(G.litName "geommin") Nothing geometryParser)

intersectsGeomNbandInput ::
  forall pgKind m n r.
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  m (Parser 'Input n (STIntersectsGeomminNband (UnpreparedValue ('Postgres pgKind))))
intersectsGeomNbandInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser <- columnParser (ColumnScalar PGInteger) (G.Nullability False)
  pure $
    P.object $$(G.litName "st_intersects_geom_nband_input") Nothing $
      STIntersectsGeomminNband
        <$> (mkParameter <$> P.field $$(G.litName "geommin") Nothing geometryParser)
        <*> (fmap mkParameter <$> P.fieldOptional $$(G.litName "nband") Nothing integerParser)

countTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column ('Postgres pgKind))) ->
  InputFieldsParser n (IR.CountDistinct -> CountType ('Postgres pgKind))
countTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional $$(G.litName "columns") Nothing (P.list columnEnum)
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [Column ('Postgres pgKind)] -> CountType ('Postgres pgKind)
    mkCountType _ Nothing = PG.CTStar
    mkCountType IR.SelectCountDistinct (Just cols) = PG.CTDistinct cols
    mkCountType IR.SelectCountNonDistinct (Just cols) = PG.CTSimple cols

-- | Update operator that prepends a value to a column containing jsonb arrays.
--
-- Note: Currently this is Postgres specific because json columns have not been ported
-- to other backends yet.
prependOp ::
  forall pgKind m n r.
  ( BackendSchema ('Postgres pgKind),
    MonadReader r m,
    MonadError QErr m,
    MonadSchema n m,
    Has MkTypename r
  ) =>
  SU.UpdateOperator ('Postgres pgKind) m n (UnpreparedValue ('Postgres pgKind))
prependOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName tableName columns = do
      let typedParser columnInfo =
            fmap P.mkParameter
              <$> BS.columnParser
                (ciType columnInfo)
                (G.Nullability $ ciIsNullable columnInfo)

          desc = "prepend existing jsonb value of filtered columns with new jsonb value"

      SU.updateOperator
        tableGQLName
        $$(G.litName "_prepend")
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
  ( BackendSchema ('Postgres pgKind),
    MonadReader r m,
    MonadError QErr m,
    MonadSchema n m,
    Has MkTypename r
  ) =>
  SU.UpdateOperator ('Postgres pgKind) m n (UnpreparedValue ('Postgres pgKind))
appendOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName tableName columns = do
      let typedParser columnInfo =
            fmap P.mkParameter
              <$> BS.columnParser
                (ciType columnInfo)
                (G.Nullability $ ciIsNullable columnInfo)

          desc = "append existing jsonb value of filtered columns with new jsonb value"
      SU.updateOperator
        tableGQLName
        $$(G.litName "_append")
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
  ( BackendSchema ('Postgres pgKind),
    MonadReader r m,
    MonadError QErr m,
    MonadSchema n m,
    Has MkTypename r
  ) =>
  SU.UpdateOperator ('Postgres pgKind) m n (UnpreparedValue ('Postgres pgKind))
deleteKeyOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName tableName columns = do
      let nullableTextParser _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGText) (G.Nullability True)
          desc = "delete key/value pair or string element. key/value pairs are matched based on their key value"

      SU.updateOperator
        tableGQLName
        $$(G.litName "_delete_key")
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
  ( BackendSchema ('Postgres pgKind),
    MonadReader r m,
    MonadError QErr m,
    MonadSchema n m,
    Has MkTypename r
  ) =>
  SU.UpdateOperator ('Postgres pgKind) m n (UnpreparedValue ('Postgres pgKind))
deleteElemOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName tableName columns = do
      let nonNullableIntParser _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGInteger) (G.Nullability False)
          desc =
            "delete the array element with specified index (negative integers count from the end). "
              <> "throws an error if top level container is not an array"

      SU.updateOperator
        tableGQLName
        $$(G.litName "_delete_elem")
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
  ( BackendSchema ('Postgres pgKind),
    MonadReader r m,
    MonadError QErr m,
    MonadSchema n m,
    Has MkTypename r
  ) =>
  SU.UpdateOperator ('Postgres pgKind) m n [UnpreparedValue ('Postgres pgKind)]
deleteAtPathOp = SU.UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isScalarColumnWhere (== PGJSONB) . ciType

    updateOperatorParser tableGQLName tableName columns = do
      let nonNullableTextListParser _ = P.list . fmap P.mkParameter <$> columnParser (ColumnScalar PGText) (G.Nullability False)
          desc = "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"

      SU.updateOperator
        tableGQLName
        $$(G.litName "_delete_at_path")
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
  m (InputFieldsParser n (HashMap (Column ('Postgres pgKind)) (UpdateOpExpression (UnpreparedValue ('Postgres pgKind)))))
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
