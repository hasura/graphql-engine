{-# LANGUAGE QuasiQuotes #-}

-- | This module is a stub/example for how to write tests for the update field
-- parsers.
--
-- Please see Test.Parser.Expectation for how to build these tests.
module Hasura.GraphQL.Schema.Build.UpdateSpec (spec) where

import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Hasura.RQL.Types.Instances ()
import Language.GraphQL.Draft.Syntax qualified as Syntax
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
                    utbWhere = [(P.nameColumnBuilder, [AEQ True P.textOld])],
                    utbUpdate = UpdateTable [(P.nameColumnBuilder, UpdateSet P.textNew)]
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
                    utbWhere = [(P.nameColumnBuilder, [AEQ True P.textOld])],
                    utbUpdate =
                      UpdateTable
                        [ (P.nameColumnBuilder, UpdateSet P.textNew),
                          (P.descColumnBuilder, UpdateSet P.textOther)
                        ]
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
              utsColumns = [P.nameColumnBuilder, P.descColumnBuilder, P.idColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerOne])],
                              mrubUpdate =
                                [ (P.nameColumnBuilder, UpdateSet P.textNew),
                                  (P.descColumnBuilder, UpdateSet P.textOther)
                                ]
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
              utsColumns = [P.nameColumnBuilder, P.descColumnBuilder, P.idColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerOne])],
                              mrubUpdate =
                                [ (P.nameColumnBuilder, UpdateSet P.textNew),
                                  (P.descColumnBuilder, UpdateSet P.textOther)
                                ]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerTwo])],
                              mrubUpdate = [(P.descColumnBuilder, UpdateSet P.textOther)]
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
              utsColumns = [P.nameColumnBuilder, P.descColumnBuilder, P.idColumnBuilder],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerOne])],
                              mrubUpdate = [(P.nameColumnBuilder, UpdateSet P.textNew)]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerOne])],
                              mrubUpdate = [(P.nameColumnBuilder, UpdateSet P.textOld)]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(P.idColumnBuilder, [AEQ True P.integerTwo])],
                              mrubUpdate = [(P.nameColumnBuilder, UpdateSet P.textOther)]
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
