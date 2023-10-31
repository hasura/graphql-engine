{-# LANGUAGE QuasiQuotes #-}

-- | This module contain unit tests of the schema of the default implementation
-- of aggregation predicates.
module Hasura.GraphQL.Schema.BoolExp.AggregationPredicatesSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.Has (Has (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Text.NonEmpty (nonEmptyTextQQ)
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Backends.Postgres.SQL.Types
  ( PGScalarType (PGInteger, PGText),
    QualifiedTable,
  )
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.GraphQL.Parser.Internal.Input (ifParser)
import Hasura.GraphQL.Schema.BoolExp.AggregationPredicates
  ( ArgumentsSignature (..),
    FunctionSignature (..),
    defaultAggregationPredicatesParser,
  )
import Hasura.GraphQL.Schema.Introspection (queryInputFieldsParserIntrospection)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.AggregationPredicates
import Hasura.RQL.IR.Value (Provenance (FreshVar), UnpreparedValue (UVParameter))
import Hasura.RQL.Types.BackendType (BackendSourceKind (PostgresVanillaKind), BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnType (ColumnScalar), ColumnValue (..))
import Hasura.RQL.Types.Common (InsertOrder (..), RelName (..), RelType (..), SourceName (..))
import Hasura.RQL.Types.NamingCase (NamingCase (..))
import Hasura.RQL.Types.Relationships.Local (RelInfo (..), RelMapping (..), RelTarget (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..), SourceInfo (..))
import Hasura.RQL.Types.SourceCustomization (ResolvedSourceCustomization (..))
import Hasura.Table.Cache
  ( TableCoreInfoG (_tciName),
    TableInfo (_tiCoreInfo),
  )
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Expectation (shouldBeSubsetOf)
import Test.Hspec
import Test.Hspec.Extended
import Test.Parser.Field qualified as GQL
import Test.Parser.Internal
  ( ColumnInfoBuilder (..),
    TableInfoBuilder (columns, relations),
    buildTableInfo,
    mkTable,
    tableInfoBuilder,
  )
import Test.Parser.Monad
  ( ParserTest (runParserTest),
    notImplementedYet,
    runSchemaTest,
  )
import Type.Reflection (Typeable, typeRep)

{- Notes:

AggregationPredicates are defined as a standalone feature. It should be possible
to test them without reference to an existing backend.

We cannot do that however, since backends have the closed datakind `BackendType`.

-}

newtype Unshowable a = Unshowable {unUnshowable :: a}
  deriving (Eq, Ord)

instance (Typeable a) => Show (Unshowable a) where
  show _ = "Unshowable<" ++ show (typeRep @a) ++ ">"

spec :: Spec
spec = do
  describe "Aggregation Predicates Schema Parsers" do
    describe "When no aggregation functions are given" do
      it "Yields no parsers" do
        let maybeParser =
              runSchemaTest sourceInfo
                $ defaultAggregationPredicatesParser @('Postgres 'Vanilla) @_ @_ @ParserTest
                  []
                  albumTableInfo
        (Unshowable maybeParser) `shouldSatisfy` (isNothing . unUnshowable)

    describe "When some aggregation functions are given" do
      let maybeParser =
            runSchemaTest sourceInfo
              $ defaultAggregationPredicatesParser @('Postgres 'Vanilla) @_ @_ @ParserTest
                [ FunctionSignature
                    { fnName = "count",
                      fnGQLName = [G.name|count|],
                      fnArguments = ArgumentsStar,
                      fnReturnType = PGInteger
                    }
                ]
                albumTableInfo

      it "Positively yields a parser" do
        (Unshowable maybeParser) `shouldSatisfy` (isJust . unUnshowable)

      dependentSpec maybeParser $ do
        it "Defines the expected GraphQL types" \parser -> do
          introspectionResult <-
            queryInputFieldsParserIntrospection
              parser
              [GQL.field|
              __schema {
                types {
                  name
                  fields {
                    name
                    type { name }
                    }
                  inputFields {
                    name
                    type { name }
                  }
                }
              }  |]

          let expectedTopLevel =
                [aesonQQ|
                { "types": [
                  {
                    "name": "track_aggregate_bool_exp",
                    "fields": null,
                    "inputFields": [
                      {
                        "name": "count",
                        "type": {
                          "name": "track_aggregate_bool_exp_count"
                        }
                      }
                    ]
                  }
                  ]
                }
                |]
          let expectedCountType =
                [aesonQQ|
                { "types": [
                  {
                    "name": "track_aggregate_bool_exp_count",
                    "fields": null,
                    "inputFields": [
                      {
                        "name": "arguments",
                        "type": {
                          "name": null
                        }
                      },
                      {
                        "name": "distinct",
                        "type": {
                          "name": "Boolean"
                        }
                      },
                      {
                        "name": "filter",
                        "type": {
                          "name": "track_bool_exp"
                        }
                      },
                      {
                        "name": "predicate",
                        "type": {
                          "name": null
                        }
                      }
                    ]
                  }
                  ]
                }
                |]

          expectedTopLevel `shouldBeSubsetOf` introspectionResult
          expectedCountType `shouldBeSubsetOf` introspectionResult

        it "Parses an example field" \parser -> do
          let input =
                [GQL.inputfields|
                tracks_aggregate: {
                  count: {
                    arguments: [],
                    predicate: {_eq : 42 },
                    distinct: true
                  }
                }
              |]
          (actual : _) <- runParserTest $ ifParser parser input

          let expected :: AggregationPredicatesImplementation ('Postgres 'Vanilla) (UnpreparedValue ('Postgres 'Vanilla))
              expected =
                AggregationPredicatesImplementation
                  { aggRelation = tracksRel,
                    aggRowPermission = BoolAnd [],
                    aggPredicate =
                      AggregationPredicate
                        { aggPredFunctionName = "count",
                          aggPredDistinct = True,
                          aggPredFilter = Nothing,
                          aggPredArguments = AggregationPredicateArgumentsStar,
                          aggPredPredicate =
                            [ AEQ
                                NonNullableComparison
                                ( UVParameter
                                    FreshVar
                                    ColumnValue
                                      { cvType = ColumnScalar PGInteger,
                                        cvValue = PGValInteger 42
                                      }
                                )
                            ]
                        }
                  }

          -- Permissions aren't in scope for this test.
          actual {aggRowPermission = BoolAnd []} `shouldBe` expected

    describe "When SchemaOptions dictate exclusion of aggregation predicates" do
      it "Yields no parsers" do
        let maybeParser =
              runSchemaTest sourceInfo
                $ local
                  ( modifier
                      ( \so ->
                          so
                            { Options.soIncludeAggregationPredicates = Options.Don'tIncludeAggregationPredicates
                            }
                      )
                  )
                $ defaultAggregationPredicatesParser @('Postgres 'Vanilla) @_ @_ @ParserTest
                  [ FunctionSignature
                      { fnName = "count",
                        fnGQLName = [G.name|count|],
                        fnArguments = ArgumentsStar,
                        fnReturnType = PGInteger
                      }
                  ]
                  albumTableInfo
        (Unshowable maybeParser) `shouldSatisfy` (isNothing . unUnshowable)
  where
    albumTableInfo :: TableInfo ('Postgres 'Vanilla)
    albumTableInfo =
      buildTableInfo
        ( (tableInfoBuilder (mkTable "album"))
            { columns =
                [ ColumnInfoBuilder
                    { cibName = "id",
                      cibPosition = 0,
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = True
                    },
                  ColumnInfoBuilder
                    { cibName = "title",
                      cibPosition = 1,
                      cibType = ColumnScalar PGText,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    }
                ],
              relations = [tracksRel]
            }
        )

    trackTableInfo :: TableInfo ('Postgres 'Vanilla)
    trackTableInfo =
      buildTableInfo
        ( (tableInfoBuilder (mkTable "track"))
            { columns =
                [ ColumnInfoBuilder
                    { cibName = "id",
                      cibPosition = 0,
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = True
                    },
                  ColumnInfoBuilder
                    { cibName = "title",
                      cibPosition = 1,
                      cibType = ColumnScalar PGText,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    },
                  ColumnInfoBuilder
                    { cibName = "duration_seconds",
                      cibPosition = 2,
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    },
                  ColumnInfoBuilder
                    { cibName = "album_id",
                      cibPosition = 3,
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    }
                ]
            }
        )

    tracksRel :: RelInfo ('Postgres 'Vanilla)
    tracksRel =
      RelInfo
        { riName = RelName [nonEmptyTextQQ|tracks|],
          riType = ArrRel,
          riMapping = RelMapping $ HashMap.fromList [("id", "album_id")],
          riTarget = RelTargetTable (mkTable "track"),
          riIsManual = False,
          riInsertOrder = AfterParent
        }

    sourceInfo :: SourceInfo ('Postgres 'Vanilla)
    sourceInfo =
      SourceInfo
        { _siName = SNDefault,
          _siSourceKind = PostgresVanillaKind,
          _siTables = makeTableCache [albumTableInfo, trackTableInfo],
          _siFunctions = mempty,
          _siNativeQueries = mempty,
          _siStoredProcedures = mempty,
          _siLogicalModels = mempty,
          _siConfiguration = notImplementedYet "SourceConfig",
          _siQueryTagsConfig = Nothing,
          _siCustomization = ResolvedSourceCustomization mempty mempty HasuraCase Nothing,
          _siDbObjectsIntrospection = DBObjectsIntrospection mempty mempty mempty mempty
        }

    makeTableCache :: [TableInfo ('Postgres 'Vanilla)] -> HashMap QualifiedTable (TableInfo ('Postgres 'Vanilla))
    makeTableCache tables = HashMap.fromList [(_tciName $ _tiCoreInfo ti, ti) | ti <- tables]
