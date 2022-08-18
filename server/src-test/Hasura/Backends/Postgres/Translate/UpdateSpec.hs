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
          where_ = [(P.idColumn, [AEQ True P.integerValue])],
          update = Expect.UpdateTable [(P.nameColumn, UpdateSet P.textValue)],
          expectedSQL =
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
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [(P.idColumn, [AEQ True P.integerValue])],
          update =
            Expect.UpdateTable
              [ (P.nameColumn, UpdateSet P.textValue),
                (P.descColumn, UpdateSet P.textValue)
              ],
          expectedSQL =
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
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ =
            [ (P.idColumn, [AEQ True P.integerValue]),
              (P.nameColumn, [AEQ False P.textValue])
            ],
          update = Expect.UpdateTable [(P.nameColumn, UpdateSet P.textValue)],
          expectedSQL =
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
          columns = [P.idColumn, P.nameColumn, P.descColumn],
          mutationOutput = MOutMultirowFields [("affected_rows", MCount)],
          where_ = [],
          update =
            Expect.UpdateMany $
              [ Expect.MultiRowUpdateBuilder
                  { mrubWhere =
                      [ (P.idColumn, [AEQ True P.integerValue]),
                        (P.nameColumn, [AEQ False P.integerValue])
                      ],
                    mrubUpdate = [(P.nameColumn, UpdateSet P.textValue)]
                  },
                Expect.MultiRowUpdateBuilder
                  { mrubWhere = [(P.idColumn, [AEQ True P.integerValue])],
                    mrubUpdate = [(P.descColumn, UpdateSet P.textValue)]
                  }
              ],
          expectedSQL =
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
