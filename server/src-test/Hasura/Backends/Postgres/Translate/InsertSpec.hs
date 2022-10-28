{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.InsertSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Test.Backend.Postgres.Insert qualified as Test
import Test.Backend.Postgres.Misc qualified as P
import Test.Hspec
import Test.Parser.Expectation as Expect

-- | TODO: add tests for OnConflict. Start with no longer passing 'Nothing' to
-- /iqp1Conflict/ in 'Test.Parser.Insert.mkInsertQuery'.
spec :: Spec
spec =
  describe "Postgres.Translate.InsertSpec" do
    Test.runTest
      Test.TestBuilder
        { name = "insert record",
          table = Expect.mkTable "test",
          insertColumns = [P.nameColumn],
          values = [[P.textNew], [P.textOther]],
          columns = [P.idColumn, P.nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          expectedSQL =
            [QQ.sql|
INSERT INTO "public"."test"
  ("name")
  VALUES
    (('new name')::text),
    (('other')::text)
  RETURNING *, ('true')::boolean AS "check__constraint"
              |]
        }
