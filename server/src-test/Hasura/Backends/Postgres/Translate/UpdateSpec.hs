{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.UpdateSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo, ColumnType (..), ColumnValue (..))
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Test.Backend.Postgres.Update qualified as Test
import Test.Hspec
import Test.Parser.Expectation qualified as Expect

type PG = 'Postgres 'Vanilla

spec :: Spec
spec =
  describe "Postgres.Translate.UpdateSpec" do
    Test.runTest
      Test.TestBuilder
        { name = "set field where id",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn],
          output = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(idColumn, [AEQ True integerOne])],
          update = Expect.UpdateTable [(nameColumn, UpdateSet newValue)],
          expected =
            [QQ.sql|
UPDATE "public"."test"
  SET "name" = $1
  WHERE
    (("public"."test"."id") = ($0))
  RETURNING * , ('true')::boolean AS "check__constraint"
              |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "set multiple field where id",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn, descColumn],
          output = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(idColumn, [AEQ True integerOne])],
          update = Expect.UpdateTable [(nameColumn, UpdateSet newValue), (descColumn, UpdateSet newValue)],
          expected =
            [QQ.sql|
UPDATE "public"."test"
  SET "name" = $1, "description" = $2
  WHERE
    (("public"."test"."id") = ($0))
  RETURNING * , ('true')::boolean AS "check__constraint"
              |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "set field where id and old value",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn, descColumn],
          output = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(idColumn, [AEQ True integerOne]), (nameColumn, [AEQ False oldValue])],
          update = Expect.UpdateTable [(nameColumn, UpdateSet newValue)],
          expected =
            [QQ.sql|
UPDATE "public"."test"
  SET "name" = $2
  WHERE
    ((("public"."test"."id") = ($0))
      AND
     ((("public"."test"."name") = ($1))
       OR
      ((("public"."test"."name") IS NULL)
        AND (($1) IS NULL))
      ))
  RETURNING * , ('true')::boolean AS "check__constraint"
              |]
        }

    Test.runMultipleUpdates
      Test.TestBuilder
        { name = "update_many with two updates",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn, descColumn],
          output = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [],
          update =
            Expect.UpdateMany $
              [ Expect.MultiRowUpdateBuilder
                  { mrubWhere = [(idColumn, [AEQ True integerOne]), (nameColumn, [AEQ False oldValue])],
                    mrubUpdate = [(nameColumn, UpdateSet newValue)]
                  },
                Expect.MultiRowUpdateBuilder
                  { mrubWhere = [(idColumn, [AEQ True integerOne])],
                    mrubUpdate = [(descColumn, UpdateSet oldValue)]
                  }
              ],
          expected =
            [ [QQ.sql|
UPDATE "public"."test"
  SET "name" = $2
  WHERE
    ((("public"."test"."id") = ($0))
      AND
     ((("public"."test"."name") = ($1))
       OR
      ((("public"."test"."name") IS NULL)
        AND (($1) IS NULL))
      ))
  RETURNING * , ('true')::boolean AS "check__constraint"
              |],
              [QQ.sql|
UPDATE "public"."test"
  SET "description" = $4
  WHERE
    (("public"."test"."id") = ($3))
  RETURNING * , ('true')::boolean AS "check__constraint"
              |]
            ]
        }

idColumn :: ColumnInfo PG
idColumn =
  Expect.mkColumnInfo
    Expect.ColumnInfoBuilder
      { cibName = "id",
        cibType = ColumnScalar PGInteger,
        cibNullable = False,
        cibIsPrimaryKey = True
      }

nameColumn :: ColumnInfo PG
nameColumn =
  Expect.mkColumnInfo
    Expect.ColumnInfoBuilder
      { cibName = "name",
        cibType = ColumnScalar PGText,
        cibNullable = False,
        cibIsPrimaryKey = False
      }

descColumn :: ColumnInfo PG
descColumn =
  Expect.mkColumnInfo
    Expect.ColumnInfoBuilder
      { cibName = "description",
        cibType = ColumnScalar PGText,
        cibNullable = False,
        cibIsPrimaryKey = False
      }

integerOne :: UnpreparedValue PG
integerOne =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

newValue :: UnpreparedValue PG
newValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "new name"
      }

oldValue :: UnpreparedValue PG
oldValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "old name"
      }
