{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Custom Scalar Type Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.CustomScalarsSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((?~))
import Data.Aeson qualified as J
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockRequestResults (..), mockAgentGraphqlTest, mockQueryResponse)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldBeYaml)
import Hasura.Backends.DataConnector.API (ColumnName (..), ScalarType (..), ScalarValue (..), mkColumnSelector)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0.Expression
import Hasura.Prelude
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec (SpecWith, describe, shouldBe)

--------------------------------------------------------------------------------

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.runWithLocalTestEnvironment
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Mock.backendTypeMetadata)
            { Fixture.mkLocalTestEnvironment = Mock.mkLocalTestEnvironment,
              Fixture.setupTeardown = \(testEnv, mockEnv) ->
                [Mock.setupAction sourceMetadata Mock.agentConfig (testEnv, mockEnv)]
            }
        ]
    )
    tests

sourceMetadata :: J.Value
sourceMetadata =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [yaml|
        name : *source
        kind: *backendType
        tables:
          - table: [MyCustomScalarsTable]
        configuration: {}
      |]

tests :: SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests = describe "Custom scalar parsing tests" $ do
  mockAgentGraphqlTest "works with simple object query" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1) {
                MyIntColumn
                MyFloatColumn
                MyStringColumn
                MyBooleanColumn
                MyIDColumn
                MyAnythingColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse customScalarsTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
              MyFloatColumn: 3.14
              MyStringColumn: foo
              MyBooleanColumn: true
              MyIDColumn: x
              MyAnythingColumn: {}
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "MyCustomScalarsTable")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("MyIntColumn", API.ColumnField (API.ColumnName "MyIntColumn") (API.ScalarType "MyInt") Nothing),
                      ("MyFloatColumn", API.ColumnField (API.ColumnName "MyFloatColumn") (API.ScalarType "MyFloat") Nothing),
                      ("MyStringColumn", API.ColumnField (API.ColumnName "MyStringColumn") (API.ScalarType "MyString") Nothing),
                      ("MyBooleanColumn", API.ColumnField (API.ColumnName "MyBooleanColumn") (API.ScalarType "MyBoolean") Nothing),
                      ("MyIDColumn", API.ColumnField (API.ColumnName "MyIDColumn") (API.ScalarType "MyID") Nothing),
                      ("MyAnythingColumn", API.ColumnField (API.ColumnName "MyAnythingColumn") (API.ScalarType "MyAnything") Nothing)
                    ]
                    & API.qLimit
                  ?~ 1
              )
        )

  mockAgentGraphqlTest "parses scalar literals in where queries" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIntColumn: {_eq: 42},
                    MyFloatColumn: {_eq: 3.14},
                    MyStringColumn: {_eq: "foo"},
                    MyBooleanColumn: {_eq: true},
                    MyIDColumn: {_eq: "x"},
                    MyAnythingColumn: {_eq: {}}
                  }) {
                MyIntColumn
                MyFloatColumn
                MyStringColumn
                MyBooleanColumn
                MyIDColumn
                MyAnythingColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse customScalarsTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
              MyFloatColumn: 3.14
              MyStringColumn: foo
              MyBooleanColumn: true
              MyIDColumn: x
              MyAnythingColumn: {}
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "MyCustomScalarsTable")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("MyIntColumn", API.ColumnField (API.ColumnName "MyIntColumn") (API.ScalarType "MyInt") Nothing),
                      ("MyFloatColumn", API.ColumnField (API.ColumnName "MyFloatColumn") (API.ScalarType "MyFloat") Nothing),
                      ("MyStringColumn", API.ColumnField (API.ColumnName "MyStringColumn") (API.ScalarType "MyString") Nothing),
                      ("MyBooleanColumn", API.ColumnField (API.ColumnName "MyBooleanColumn") (API.ScalarType "MyBoolean") Nothing),
                      ("MyIDColumn", API.ColumnField (API.ColumnName "MyIDColumn") (API.ScalarType "MyID") Nothing),
                      ("MyAnythingColumn", API.ColumnField (API.ColumnName "MyAnythingColumn") (API.ScalarType "MyAnything") Nothing)
                    ]
                    & API.qLimit
                  ?~ 1
                    & API.qWhere
                  ?~ And
                    ( Set.fromList
                        [ ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyBooleanColumn") (ScalarType "MyBoolean") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.Bool True) (ScalarType "MyBoolean")),
                          ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyFloatColumn") (ScalarType "MyFloat") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.Number 3.14) (ScalarType "MyFloat")),
                          ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyStringColumn") (ScalarType "MyString") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.String "foo") (ScalarType "MyString")),
                          ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyIDColumn") (ScalarType "MyID") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.String "x") (ScalarType "MyID")),
                          ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyIntColumn") (ScalarType "MyInt") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.Number 42.0) (ScalarType "MyInt")),
                          ApplyBinaryComparisonOperator
                            Equal
                            (ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "MyAnythingColumn") (ScalarType "MyAnything") Nothing)
                            (ScalarValueComparison $ ScalarValue (J.Object mempty) (ScalarType "MyAnything"))
                        ]
                    )
              )
        )

  mockAgentGraphqlTest "fails parsing float when expecting int" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIntColumn: {_eq: 41.3}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
            message: expected a 32-bit integer for type 'MyInt', but found a float
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing string when expecting int" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIntColumn: {_eq: "41"}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
            message: expected a 32-bit integer for type 'MyInt', but found a string
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing boolean when expecting int" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIntColumn: {_eq: true}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
            message: expected a 32-bit integer for type 'MyInt', but found a boolean
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "succeeds parsing int when expecting float" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyFloatColumn: {_eq: 42}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "fails parsing string when expecting float" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyFloatColumn: {_eq: "foo"}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyFloatColumn._eq
            message: expected a float for type 'MyFloat', but found a string
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing boolean when expecting float" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyFloatColumn: {_eq: true}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyFloatColumn._eq
            message: expected a float for type 'MyFloat', but found a boolean
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing int when expecting string" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyStringColumn: {_eq: 42}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
            message: expected a string for type 'MyString', but found an integer
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing float when expecting string" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyStringColumn: {_eq: 3.14}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
            message: expected a string for type 'MyString', but found a float
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing boolean when expecting string" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyStringColumn: {_eq: true}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
            message: expected a string for type 'MyString', but found a boolean
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing int when expecting boolean" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyBooleanColumn: {_eq: 42}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
            message: expected a boolean for type 'MyBoolean', but found an integer
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing float when expecting boolean" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyBooleanColumn: {_eq: 3.14}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
            message: expected a boolean for type 'MyBoolean', but found a float
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing string when expecting boolean" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyBooleanColumn: {_eq: "true"}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
            message: expected a boolean for type 'MyBoolean', but found a string
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "succeeds parsing int when expecting ID" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIDColumn: {_eq: 42}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "fails parsing float when expecting ID" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIDColumn: {_eq: 3.14}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyIDColumn._eq
            message: expected a String or a 32-bit integer for type 'MyID', but found a float
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "fails parsing boolean when expecting ID" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyIDColumn: {_eq: true}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        errors:
          - extensions:
              code: validation-failed
              path: $.selectionSet.MyCustomScalarsTable.args.where.MyIDColumn._eq
            message: expected a String or a 32-bit integer for type 'MyID', but found a boolean
      |]

    _mrrRecordedRequest `shouldBe` Nothing

  mockAgentGraphqlTest "succeeds parsing int when expecting anything" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyAnythingColumn: {_eq: 42}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "succeeds parsing float when expecting anything" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyAnythingColumn: {_eq: 3.14}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "succeeds parsing string when expecting anything" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyAnythingColumn: {_eq: "foo"}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "succeeds parsing boolean when expecting anything" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyAnythingColumn: {_eq: true}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]

  mockAgentGraphqlTest "succeeds parsing array when expecting anything" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query {
              MyCustomScalarsTable(limit: 1, where: {
                    MyAnythingColumn: {_eq: ["a", 1, false]}
                  }) {
                MyIntColumn
              }
            }
          |]
    let mockConfig = mockQueryResponse (mkRowsQueryResponse myIntTable)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          MyCustomScalarsTable:
            - MyIntColumn: 42
      |]
  where
    customScalarsTable =
      [ [ ("MyIntColumn", API.mkColumnFieldValue $ J.Number 42),
          ("MyFloatColumn", API.mkColumnFieldValue $ J.Number 3.14),
          ("MyStringColumn", API.mkColumnFieldValue $ J.String "foo"),
          ("MyBooleanColumn", API.mkColumnFieldValue $ J.Bool True),
          ("MyIDColumn", API.mkColumnFieldValue $ J.String "x"),
          ("MyAnythingColumn", API.mkColumnFieldValue $ J.Object mempty)
        ]
      ]
    myIntTable =
      [ [ ("MyIntColumn", API.mkColumnFieldValue $ J.Number 42)
        ]
      ]
