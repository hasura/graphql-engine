{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Translate.UpdateSpec
  ( spec,
  )
where

import Database.PG.Query.Pool qualified as QQ
import Hasura.Backends.Postgres.Types.Update (UpdateOpExpression (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
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
          where_ = [(P.idColumn, [AEQ True P.integerOne])],
          update = Expect.UpdateTable [(P.nameColumn, UpdateSet P.textNew)],
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
          where_ = [(P.idColumn, [AEQ True P.integerOne])],
          update =
            Expect.UpdateTable
              [ (P.nameColumn, UpdateSet P.textNew),
                (P.descColumn, UpdateSet P.textOther)
              ],
          expectedSQL =
            [QQ.sql|
UPDATE "public"."test"
  SET "name" = ('new name')::text, "description" = ('other')::text
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
          where_ =
            [ (P.idColumn, [AEQ True P.integerOne]),
              (P.nameColumn, [AEQ False P.textOld])
            ],
          update = Expect.UpdateTable [(P.nameColumn, UpdateSet P.textNew)],
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
          where_ = [],
          update =
            Expect.UpdateMany $
              [ Expect.MultiRowUpdateBuilder
                  { mrubWhere =
                      [ (P.idColumn, [AEQ True P.integerOne]),
                        (P.nameColumn, [AEQ False P.textNew])
                      ],
                    mrubUpdate = [(P.nameColumn, UpdateSet P.textNew)]
                  },
                Expect.MultiRowUpdateBuilder
                  { mrubWhere = [(P.idColumn, [AEQ True P.integerOne])],
                    mrubUpdate = [(P.descColumn, UpdateSet P.textNew)]
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
