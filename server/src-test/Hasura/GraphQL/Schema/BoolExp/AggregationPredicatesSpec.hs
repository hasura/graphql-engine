{-# LANGUAGE QuasiQuotes #-}

-- | This module contain unit tests of the schema of the default implementation
-- of aggregation predicates.
module Hasura.GraphQL.Schema.BoolExp.AggregationPredicatesSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.HashMap.Strict qualified as HM
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
import Hasura.GraphQL.Schema.NamingCase (NamingCase (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..), OpExpG (AEQ))
import Hasura.RQL.IR.BoolExp.AggregationPredicates
import Hasura.RQL.IR.Value (UnpreparedValue (UVParameter))
import Hasura.RQL.Types.Column (ColumnType (ColumnScalar), ColumnValue (..))
import Hasura.RQL.Types.Common (InsertOrder (..), RelName (..), RelType (..), SourceName (..))
import Hasura.RQL.Types.Relationships.Local (RelInfo (..))
import Hasura.RQL.Types.Source (SourceInfo (..))
import Hasura.RQL.Types.SourceCustomization (ResolvedSourceCustomization (..))
import Hasura.RQL.Types.Table
  ( TableCoreInfoG (_tciName),
    TableInfo (_tiCoreInfo),
  )
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Expectation (shouldBeSubsetOf)
import Test.Hspec
import Test.Hspec.Extended
import Test.Parser.Field qualified as GQL
import Test.Parser.Internal
  ( ColumnInfoBuilder
      ( ColumnInfoBuilder,
        cibIsPrimaryKey,
        cibName,
        cibNullable,
        cibType
      ),
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

instance Typeable a => Show (Unshowable a) where
  show _ = "Unshowable<" ++ show (typeRep @a) ++ ">"

spec :: Spec
spec = do
  describe "Aggregation Predicates Schema Parsers" do
    describe "When no aggregation functions are given" do
      it "Yields no parsers" do
        let maybeParser =
              runSchemaTest $
                defaultAggregationPredicatesParser @('Postgres 'Vanilla) @_ @_ @ParserTest
                  []
                  sourceInfo
                  albumTableInfo
        (Unshowable maybeParser) `shouldSatisfy` (isNothing . unUnshowable)

    describe "When some aggregation functions are given" do
      let maybeParser =
            runSchemaTest $
              defaultAggregationPredicatesParser @('Postgres 'Vanilla) @_ @_ @ParserTest
                [ FunctionSignature
                    { fnName = "count",
                      fnGQLName = [G.name|count|],
                      fnArguments = ArgumentsStar,
                      fnReturnType = PGInteger
                    }
                ]
                sourceInfo
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
                    "name": "album_tracks_aggregate",
                    "fields": null,
                    "inputFields": [
                      {
                        "name": "count",
                        "type": {
                          "name": "album_tracks_aggregate_count"
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
                    "name": "album_tracks_aggregate_count",
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
                                True
                                ( UVParameter
                                    Nothing
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
  where
    albumTableInfo :: TableInfo ('Postgres 'Vanilla)
    albumTableInfo =
      buildTableInfo
        ( (tableInfoBuilder (mkTable "album"))
            { columns =
                [ ColumnInfoBuilder
                    { cibName = "id",
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = True
                    },
                  ColumnInfoBuilder
                    { cibName = "title",
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
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = True
                    },
                  ColumnInfoBuilder
                    { cibName = "title",
                      cibType = ColumnScalar PGText,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    },
                  ColumnInfoBuilder
                    { cibName = "duration_seconds",
                      cibType = ColumnScalar PGInteger,
                      cibNullable = False,
                      cibIsPrimaryKey = False
                    },
                  ColumnInfoBuilder
                    { cibName = "album_id",
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
          riMapping = HM.fromList [("id", "album_id")],
          riRTable = (mkTable "track"),
          riIsManual = False,
          riInsertOrder = AfterParent
        }

    sourceInfo :: SourceInfo ('Postgres 'Vanilla)
    sourceInfo =
      SourceInfo
        { _siName = SNDefault,
          _siTables = makeTableCache [albumTableInfo, trackTableInfo],
          _siFunctions = mempty,
          _siConfiguration = notImplementedYet "SourceConfig",
          _siQueryTagsConfig = Nothing,
          _siCustomization = ResolvedSourceCustomization mempty mempty HasuraCase Nothing
        }

    makeTableCache :: [TableInfo ('Postgres 'Vanilla)] -> HashMap QualifiedTable (TableInfo ('Postgres 'Vanilla))
    makeTableCache tables = HM.fromList [(_tciName $ _tiCoreInfo ti, ti) | ti <- tables]
