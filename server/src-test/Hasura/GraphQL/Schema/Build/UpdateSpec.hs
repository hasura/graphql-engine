{-# LANGUAGE QuasiQuotes #-}

-- | This module is a stub/example for how to write tests for the update field
-- parsers.
--
-- Please see Test.Parser.Expectation for how to build these tests.
module Hasura.GraphQL.Schema.Build.UpdateSpec (spec) where

import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Hasura.RQL.Types.Instances ()
import Test.Backend.Postgres.Misc qualified as P
import Test.Hspec
import Test.Parser.Expectation
import Test.Parser.Field qualified as GQL

-- | These tests are samples and happy path testers.
--
-- Given a table with one or two columns, perform a simple update. There are no
-- permission restrictions. It's also only using text fields and 'UpdateSet'.
spec :: Spec
spec = do
  describe "Update parsers" do
    describe "update where" do
      it "single column" do
        runUpdateFieldTest
          UpdateTestSetup
            { utsTable = "artist",
              utsColumns = [P.nameColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbUpdate =
                      SingleBatchUpdate
                        $ UpdateBatchBuilder
                          { ubbOperations = [(P.nameColumnBuilder, UpdateSet P.textNew)],
                            ubbWhere = [(P.nameColumnBuilder, [AEQ NonNullableComparison P.textOld])]
                          }
                  },
              utsField =
                [GQL.field|
                  update_artist(
                    where: { name: { _eq: "old name"}},
                    _set: { name: "new name" }
                  ) {
                    affected_rows
                  }
                |]
            }

      it "two columns" do
        runUpdateFieldTest
          UpdateTestSetup
            { utsTable = "artist",
              utsColumns = [P.nameColumnBuilder, P.descColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbUpdate =
                      SingleBatchUpdate
                        $ UpdateBatchBuilder
                          { ubbOperations =
                              [ (P.nameColumnBuilder, UpdateSet P.textNew),
                                (P.descColumnBuilder, UpdateSet P.textOther)
                              ],
                            ubbWhere = [(P.nameColumnBuilder, [AEQ NonNullableComparison P.textOld])]
                          }
                  },
              utsField =
                [GQL.field|
                  update_artist(
                    where: { name: { _eq: "old name"}},
                    _set: { name: "new name", description: "other" }
                  ) {
                    affected_rows
                  }
                |]
            }

    describe "update many" do
      it "one update" do
        runUpdateFieldTest
          UpdateTestSetup
            { utsTable = "artist",
              utsColumns = [P.idColumnBuilder, P.nameColumnBuilder, P.descColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbUpdate =
                      MultipleBatchesUpdate
                        [ UpdateBatchBuilder
                            { ubbOperations =
                                [ (P.nameColumnBuilder, UpdateSet P.textNew),
                                  (P.descColumnBuilder, UpdateSet P.textOther)
                                ],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerOne])]
                            }
                        ]
                  },
              utsField =
                [GQL.field|
                  update_artist_many(
                    updates: [
                      { where: { id: { _eq: 1 } },
                        _set: { name: "new name", description: "other" }
                      }
                      ]
                  ) {
                    affected_rows
                  }
                |]
            }

      it "two updates, complex where clause" do
        runUpdateFieldTest
          UpdateTestSetup
            { utsTable = "artist",
              utsColumns = [P.idColumnBuilder, P.nameColumnBuilder, P.descColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbUpdate =
                      MultipleBatchesUpdate
                        [ UpdateBatchBuilder
                            { ubbOperations =
                                [ (P.nameColumnBuilder, UpdateSet P.textNew),
                                  (P.descColumnBuilder, UpdateSet P.textOther)
                                ],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerOne])]
                            },
                          UpdateBatchBuilder
                            { ubbOperations = [(P.descColumnBuilder, UpdateSet P.textOther)],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerTwo])]
                            }
                        ]
                  },
              utsField =
                [GQL.field|
                  update_artist_many(
                    updates: [
                      { where: { id: { _eq: 1 } }
                        _set: { name: "new name", description: "other" }
                      }
                      { where: { id: { _eq: 2 } }
                        _set: { description: "other" }
                      }
                      ]
                  ) {
                    affected_rows
                  }
                |]
            }

      it "three updates, ordering" do
        runUpdateFieldTest
          UpdateTestSetup
            { utsTable = "artist",
              utsColumns = [P.idColumnBuilder, P.nameColumnBuilder, P.descColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbUpdate =
                      MultipleBatchesUpdate
                        [ UpdateBatchBuilder
                            { ubbOperations = [(P.nameColumnBuilder, UpdateSet P.textNew)],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerOne])]
                            },
                          UpdateBatchBuilder
                            { ubbOperations = [(P.nameColumnBuilder, UpdateSet P.textOld)],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerOne])]
                            },
                          UpdateBatchBuilder
                            { ubbOperations = [(P.nameColumnBuilder, UpdateSet P.textOther)],
                              ubbWhere = [(P.idColumnBuilder, [AEQ NonNullableComparison P.integerTwo])]
                            }
                        ]
                  },
              utsField =
                [GQL.field|
                  update_artist_many(
                    updates: [
                      { where: { id: { _eq: 1 } }
                        _set: { name: "new name" }
                      }
                      { where: { id: { _eq: 1 } }
                        _set: { name: "old name" }
                      }
                      { where: { id: { _eq: 2 } }
                        _set: { name: "other" }
                      }
                      ]
                  ) {
                    affected_rows
                  }
                |]
            }
