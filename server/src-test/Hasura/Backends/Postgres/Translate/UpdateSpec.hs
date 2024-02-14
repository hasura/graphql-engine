{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.UpdateSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning (MutFldG (..), MutationOutputG (..))
import Test.Backend.Postgres.Misc qualified as P
import Test.Backend.Postgres.Update qualified as Test
import Test.Hspec
import Test.Parser.Expectation qualified as Expect

spec :: Spec
spec =
  describe "Postgres.Translate.UpdateSpec" do
    Test.runTest
      Test.TestBuilder
        { name = "set field where id",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          updateVariant =
            Expect.SingleBatchUpdate
              $ Expect.UpdateBatchBuilder
                { ubbOperations = [(P.nameColumn, UpdateSet P.textNew)],
                  ubbWhere = [(P.idColumn, [AEQ NonNullableComparison P.integerOne])]
                },
          expectedSQL =
            [QQ.sql|
              UPDATE "public"."test"
                SET "name" = ('new name'):: text
                WHERE
                  (("public"."test"."id") = (('1')::integer))
                RETURNING * , ('true')::boolean AS "check__constraint"
            |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "set multiple field where id",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          updateVariant =
            Expect.SingleBatchUpdate
              $ Expect.UpdateBatchBuilder
                { ubbOperations =
                    [ (P.nameColumn, UpdateSet P.textNew),
                      (P.descColumn, UpdateSet P.textOther)
                    ],
                  ubbWhere = [(P.idColumn, [AEQ NonNullableComparison P.integerOne])]
                },
          expectedSQL =
            [QQ.sql|
              UPDATE "public"."test"
                SET "description" = ('other')::text, "name" = ('new name')::text
                WHERE
                  (("public"."test"."id") = (('1')::integer))
                RETURNING * , ('true')::boolean AS "check__constraint"
            |]
        }

    Test.runTest
      Test.TestBuilder
        { name = "set field where id and old value",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          updateVariant =
            Expect.SingleBatchUpdate
              $ Expect.UpdateBatchBuilder
                { ubbOperations = [(P.nameColumn, UpdateSet P.textNew)],
                  ubbWhere =
                    [ (P.idColumn, [AEQ NonNullableComparison P.integerOne]),
                      (P.nameColumn, [AEQ NullableComparison P.textOld])
                    ]
                },
          expectedSQL =
            [QQ.sql|
              UPDATE "public"."test"
                SET "name" = ('new name')::text
                WHERE
                  ((("public"."test"."id") = (('1')::integer))
                    AND
                  ((("public"."test"."name") = (('old name')::text))
                    OR
                    ((("public"."test"."name") IS NULL)
                      AND ((('old name')::text) IS NULL))
                    ))
                RETURNING * , ('true')::boolean AS "check__constraint"
            |]
        }

    Test.runMultipleUpdates
      Test.TestBuilder
        { name = "update_many with two updates",
          table = Expect.mkTable "test",
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          updateVariant =
            Expect.MultipleBatchesUpdate
              [ Expect.UpdateBatchBuilder
                  { ubbOperations = [(P.nameColumn, UpdateSet P.textNew)],
                    ubbWhere =
                      [ (P.idColumn, [AEQ NonNullableComparison P.integerOne]),
                        (P.nameColumn, [AEQ NullableComparison P.textNew])
                      ]
                  },
                Expect.UpdateBatchBuilder
                  { ubbOperations = [(P.descColumn, UpdateSet P.textNew)],
                    ubbWhere = [(P.idColumn, [AEQ NonNullableComparison P.integerOne])]
                  }
              ],
          expectedSQL =
            [ [QQ.sql|
                UPDATE "public"."test"
                  SET "name" = ('new name')::text
                  WHERE
                    ((("public"."test"."id") = (('1')::integer))
                      AND
                    ((("public"."test"."name") = (('new name')::text))
                      OR
                      ((("public"."test"."name") IS NULL)
                        AND ((('new name')::text) IS NULL))
                      ))
                  RETURNING * , ('true')::boolean AS "check__constraint"
              |],
              [QQ.sql|
                UPDATE "public"."test"
                  SET "description" = ('new name')::text
                  WHERE
                    (("public"."test"."id") = (('1')::integer))
                  RETURNING * , ('true')::boolean AS "check__constraint"
              |]
            ]
        }
