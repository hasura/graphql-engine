{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.DeleteSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Test.Backend.Postgres.Delete qualified as Test
import Test.Backend.Postgres.Misc qualified as P
import Test.Hspec
import Test.Parser.Expectation as Expect

spec :: Spec
spec =
  describe "Postgres.Translate.DeleteSpec" do
    Test.runTest
      Test.TestBuilder
        { name = "delete where id",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(P.idColumn, [AEQ NonNullableComparison P.integerOne])],
          expectedSQL =
            [QQ.sql|
DELETE FROM "public"."test"
  WHERE
    (("public"."test"."id") = (('1')::integer))
  RETURNING *
              |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "delete where column",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(P.nameColumn, [AEQ NonNullableComparison P.textOld])],
          expectedSQL =
            [QQ.sql|
DELETE FROM "public"."test"
  WHERE
    (("public"."test"."name") = (('old name')::text))
  RETURNING *
              |]
        }
