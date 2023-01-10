{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Custom Scalar Type Tests for Data Connector Backend using a Mock Agent
module Test.DataConnector.MockAgent.CustomScalarsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Mock (TestCase (..))
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Hasura.Backends.DataConnector.API (ColumnName (..), ScalarType (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.API.V0.Expression
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

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

sourceMetadata :: Aeson.Value
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

tests :: Fixture.Options -> SpecWith (TestEnvironment, Mock.MockAgentEnvironment)
tests opts = do
  describe "Custom scalar parsing tests" $ do
    it "works with simple object query" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse customScalarsTable)},
                  _whenRequestRequired =
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
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                            MyFloatColumn: 3.14
                            MyStringColumn: foo
                            MyBooleanColumn: true
                            MyIDColumn: x
                            MyAnythingColumn: {}
                    |]
                }
         in (Mock.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("MyCustomScalarsTable" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    HashMap.fromList
                                      [ (API.FieldName "MyIntColumn", API.ColumnField (API.ColumnName "MyIntColumn") $ API.CustomTy "MyInt"),
                                        (API.FieldName "MyFloatColumn", API.ColumnField (API.ColumnName "MyFloatColumn") $ API.CustomTy "MyFloat"),
                                        (API.FieldName "MyStringColumn", API.ColumnField (API.ColumnName "MyStringColumn") $ API.CustomTy "MyString"),
                                        (API.FieldName "MyBooleanColumn", API.ColumnField (API.ColumnName "MyBooleanColumn") $ API.CustomTy "MyBoolean"),
                                        (API.FieldName "MyIDColumn", API.ColumnField (API.ColumnName "MyIDColumn") $ API.CustomTy "MyID"),
                                        (API.FieldName "MyAnythingColumn", API.ColumnField (API.ColumnName "MyAnythingColumn") $ API.CustomTy "MyAnything")
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere = Nothing,
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }
    it "parses scalar literals in where queries" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse customScalarsTable)},
                  _whenRequestRequired =
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
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                            MyFloatColumn: 3.14
                            MyStringColumn: foo
                            MyBooleanColumn: true
                            MyIDColumn: x
                            MyAnythingColumn: {}
                    |]
                }
         in (Mock.defaultTestCase required)
              { _whenQuery =
                  Just
                    ( API.QueryRequest
                        { _qrTable = API.TableName ("MyCustomScalarsTable" :| []),
                          _qrTableRelationships = [],
                          _qrQuery =
                            API.Query
                              { _qFields =
                                  Just $
                                    HashMap.fromList
                                      [ (API.FieldName "MyIntColumn", API.ColumnField (API.ColumnName "MyIntColumn") $ API.CustomTy "MyInt"),
                                        (API.FieldName "MyFloatColumn", API.ColumnField (API.ColumnName "MyFloatColumn") $ API.CustomTy "MyFloat"),
                                        (API.FieldName "MyStringColumn", API.ColumnField (API.ColumnName "MyStringColumn") $ API.CustomTy "MyString"),
                                        (API.FieldName "MyBooleanColumn", API.ColumnField (API.ColumnName "MyBooleanColumn") $ API.CustomTy "MyBoolean"),
                                        (API.FieldName "MyIDColumn", API.ColumnField (API.ColumnName "MyIDColumn") $ API.CustomTy "MyID"),
                                        (API.FieldName "MyAnythingColumn", API.ColumnField (API.ColumnName "MyAnythingColumn") $ API.CustomTy "MyAnything")
                                      ],
                                _qAggregates = Nothing,
                                _qLimit = Just 1,
                                _qOffset = Nothing,
                                _qWhere =
                                  Just $
                                    And
                                      [ ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyBooleanColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyBoolean"}})
                                          (ScalarValue (Aeson.Bool True) (CustomTy {getCustomTy = "MyBoolean"})),
                                        ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyFloatColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyFloat"}})
                                          (ScalarValue (Aeson.Number 3.14) (CustomTy {getCustomTy = "MyFloat"})),
                                        ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyStringColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyString"}})
                                          (ScalarValue (Aeson.String "foo") (CustomTy {getCustomTy = "MyString"})),
                                        ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyIDColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyID"}})
                                          (ScalarValue (Aeson.String "x") (CustomTy {getCustomTy = "MyID"})),
                                        ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyIntColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyInt"}})
                                          (ScalarValue (Aeson.Number 42.0) (CustomTy {getCustomTy = "MyInt"})),
                                        ApplyBinaryComparisonOperator
                                          Equal
                                          (ComparisonColumn {_ccPath = CurrentTable, _ccName = ColumnName {unColumnName = "MyAnythingColumn"}, _ccColumnType = CustomTy {getCustomTy = "MyAnything"}})
                                          (ScalarValue (Aeson.Object mempty) (CustomTy {getCustomTy = "MyAnything"}))
                                      ],
                                _qOrderBy = Nothing
                              }
                        }
                    )
              }
    it "fails parsing float when expecting int" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIntColumn: {_eq: 41.3}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
                          message: expected a 32-bit integer for type 'MyInt', but found a float

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing string when expecting int" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIntColumn: {_eq: "41"}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
                          message: expected a 32-bit integer for type 'MyInt', but found a string

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing boolean when expecting int" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIntColumn: {_eq: true}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyIntColumn._eq
                          message: expected a 32-bit integer for type 'MyInt', but found a boolean

                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing int when expecting float" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyFloatColumn: {_eq: 42}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing string when expecting float" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyFloatColumn: {_eq: "foo"}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyFloatColumn._eq
                          message: expected a float for type 'MyFloat', but found a string

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing boolean when expecting float" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyFloatColumn: {_eq: true}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyFloatColumn._eq
                          message: expected a float for type 'MyFloat', but found a boolean

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing int when expecting string" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyStringColumn: {_eq: 42}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
                          message: expected a string for type 'MyString', but found an integer

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing float when expecting string" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyStringColumn: {_eq: 3.14}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
                          message: expected a string for type 'MyString', but found a float

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing boolean when expecting string" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyStringColumn: {_eq: true}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyStringColumn._eq
                          message: expected a string for type 'MyString', but found a boolean

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing int when expecting boolean" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyBooleanColumn: {_eq: 42}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
                          message: expected a boolean for type 'MyBoolean', but found an integer

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing float when expecting boolean" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyBooleanColumn: {_eq: 3.14}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
                          message: expected a boolean for type 'MyBoolean', but found a float

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing string when expecting boolean" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyBooleanColumn: {_eq: "true"}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyBooleanColumn._eq
                          message: expected a boolean for type 'MyBoolean', but found a string

                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing int when expecting ID" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIDColumn: {_eq: 42}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing float when expecting ID" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIDColumn: {_eq: 3.14}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyIDColumn._eq
                          message: expected a String or a 32-bit integer for type 'MyID', but found a float

                    |]
                }
         in Mock.defaultTestCase required
    it "fails parsing boolean when expecting ID" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyIDColumn: {_eq: true}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      errors:
                        - extensions:
                            code: validation-failed
                            path: $.selectionSet.MyCustomScalarsTable.args.where.MyIDColumn._eq
                          message: expected a String or a 32-bit integer for type 'MyID', but found a boolean

                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing int when expecting anything" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyAnythingColumn: {_eq: 42}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing float when expecting anything" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyAnythingColumn: {_eq: 3.14}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing string when expecting anything" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyAnythingColumn: {_eq: "foo"}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing boolean when expecting anything" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyAnythingColumn: {_eq: true}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
    it "succeeds parsing array when expecting anything" $
      Mock.runTest opts $
        let required =
              Mock.TestCaseRequired
                { _givenRequired =
                    Mock.chinookMock {Mock._queryResponse = \_ -> Right (rowsResponse myIntTable)},
                  _whenRequestRequired =
                    [graphql|
                      query {
                        MyCustomScalarsTable(limit: 1, where: {
                              MyAnythingColumn: {_eq: ["a", 1, false]}
                            }) {
                          MyIntColumn
                        }
                      }
                    |],
                  _thenRequired =
                    [yaml|
                      data:
                        MyCustomScalarsTable:
                          - MyIntColumn: 42
                    |]
                }
         in Mock.defaultTestCase required
  where
    customScalarsTable =
      [ [ (API.FieldName "MyIntColumn", API.mkColumnFieldValue $ Aeson.Number 42),
          (API.FieldName "MyFloatColumn", API.mkColumnFieldValue $ Aeson.Number 3.14),
          (API.FieldName "MyStringColumn", API.mkColumnFieldValue $ Aeson.String "foo"),
          (API.FieldName "MyBooleanColumn", API.mkColumnFieldValue $ Aeson.Bool True),
          (API.FieldName "MyIDColumn", API.mkColumnFieldValue $ Aeson.String "x"),
          (API.FieldName "MyAnythingColumn", API.mkColumnFieldValue $ Aeson.Object mempty)
        ]
      ]
    myIntTable =
      [ [ (API.FieldName "MyIntColumn", API.mkColumnFieldValue $ Aeson.Number 42)
        ]
      ]

rowsResponse :: [[(API.FieldName, API.FieldValue)]] -> API.QueryResponse
rowsResponse rows = API.QueryResponse (Just $ HashMap.fromList <$> rows) Nothing
