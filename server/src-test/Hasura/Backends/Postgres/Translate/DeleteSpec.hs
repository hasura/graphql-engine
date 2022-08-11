{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.DeleteSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo, ColumnType (..), ColumnValue (..))
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Test.Backend.Postgres.Delete qualified as Test
import Test.Hspec
import Test.Parser.Expectation as Expect

type PG = 'Postgres 'Vanilla

spec :: Spec
spec =
  describe "Postgres.Translate.DeleteSpec" do
    Test.runTest
      Test.TestBuilder
        { name = "delete where id",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(idColumn, [AEQ True integerOne])],
          expectedSQL =
            [QQ.sql|
DELETE FROM "public"."test"
  WHERE
    (("public"."test"."id") = ($0))
  RETURNING *
              |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "delete where column",
          table = Expect.mkTable "test",
          columns = [idColumn, nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(nameColumn, [AEQ True oldValue])],
          expectedSQL =
            [QQ.sql|
DELETE FROM "public"."test"
  WHERE
    (("public"."test"."name") = ($0))
  RETURNING *
              |]
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

integerOne :: UnpreparedValue PG
integerOne =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

oldValue :: UnpreparedValue PG
oldValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "old name"
      }
