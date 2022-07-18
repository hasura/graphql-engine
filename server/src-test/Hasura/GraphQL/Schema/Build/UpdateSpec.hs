{-# LANGUAGE QuasiQuotes #-}

-- | This module is a stub/example for how to write tests for the update field
-- parsers.
--
-- Please see Test.Parser.Expectation for how to build these tests.
module Hasura.GraphQL.Schema.Build.UpdateSpec (spec) where

import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnType (..), ColumnValue (..))
import Hasura.RQL.Types.Instances ()
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Test.Hspec
import Test.Parser.Expectation
import Test.Parser.Field qualified as GQL

type PG = 'Postgres 'Vanilla

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
              utsColumns = [nameColumn],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [(nameColumn, [AEQ True oldValue])],
                    utbUpdate = UpdateTable [(nameColumn, UpdateSet newValue)]
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
              utsColumns = [nameColumn, descColumn],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [(nameColumn, [AEQ True oldValue])],
                    utbUpdate =
                      UpdateTable
                        [ (nameColumn, UpdateSet newValue),
                          (descColumn, UpdateSet otherValue)
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
              utsColumns = [nameColumn, descColumn, idColumn],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerOne])],
                              mrubUpdate =
                                [ (nameColumn, UpdateSet newValue),
                                  (descColumn, UpdateSet otherValue)
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
              utsColumns = [nameColumn, descColumn, idColumn],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerOne])],
                              mrubUpdate =
                                [ (nameColumn, UpdateSet newValue),
                                  (descColumn, UpdateSet otherValue)
                                ]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerTwo])],
                              mrubUpdate = [(descColumn, UpdateSet otherValue)]
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
              utsColumns = [nameColumn, descColumn, idColumn],
              utsExpect =
                UpdateExpectationBuilder
                  { utbOutput = MOutMultirowFields [("affected_rows", MCount)],
                    utbWhere = [],
                    utbUpdate =
                      UpdateMany
                        [ MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerOne])],
                              mrubUpdate = [(nameColumn, UpdateSet newValue)]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerOne])],
                              mrubUpdate = [(nameColumn, UpdateSet oldValue)]
                            },
                          MultiRowUpdateBuilder
                            { mrubWhere = [(idColumn, [AEQ True integerTwo])],
                              mrubUpdate = [(nameColumn, UpdateSet otherValue)]
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

idColumn :: ColumnInfoBuilder
idColumn =
  ColumnInfoBuilder
    { cibName = "id",
      cibType = ColumnScalar PGInteger,
      cibNullable = False,
      cibIsPrimaryKey = True
    }

nameColumn :: ColumnInfoBuilder
nameColumn =
  ColumnInfoBuilder
    { cibName = "name",
      cibType = ColumnScalar PGText,
      cibNullable = False,
      cibIsPrimaryKey = False
    }

oldValue :: UnpreparedValue PG
oldValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "old name"
      }

newValue :: UnpreparedValue PG
newValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "new name"
      }

descColumn :: ColumnInfoBuilder
descColumn =
  ColumnInfoBuilder
    { cibName = "description",
      cibType = ColumnScalar PGText,
      cibNullable = False,
      cibIsPrimaryKey = False
    }

otherValue :: UnpreparedValue PG
otherValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "other"
      }

integerOne :: UnpreparedValue PG
integerOne =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

integerTwo :: UnpreparedValue PG
integerTwo =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 2
      }
