-- | Test subscription multiplex query.
-- Regression reported at https://github.com/hasura/graphql-engine/issues/9281
module Test.Regression.MultiplexQuerySpec (spec) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (i)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema qualified as Schema
import Harness.Subscriptions
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Postgres.setupTablesAction [userTable] testEnv,
                  setupFunctions testEnv,
                  Postgres.setupComputedFieldAction userTable "get_age" "get_future_age" testEnv,
                  Postgres.setupFunctionRootFieldAction "fetch_users" testEnv
                ]
            }
        ]
    )
    tests

-- ** Schema

userTable :: Schema.Table
userTable =
  (Schema.table "user")
    { Schema.tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr,
          Schema.column "birth_year" Schema.TInt
        ],
      Schema.tablePrimaryKey = ["id"],
      Schema.tableData =
        [ [ Schema.VInt 1,
            Schema.VStr "user_1",
            Schema.VInt 1996
          ],
          [ Schema.VInt 2,
            Schema.VStr "user_2",
            Schema.VInt 2000
          ]
        ]
    }

-- ** Setup & Teardown

setupFunctions :: TestEnvironment -> Fixture.SetupAction
setupFunctions testEnv =
  let schemaName = Schema.getSchemaName testEnv
      get_age = Schema.unSchemaName schemaName <> ".get_age"
      fetch_users = Schema.unSchemaName schemaName <> ".fetch_users"
   in Fixture.SetupAction
        { Fixture.setupAction = do
            Postgres.run_ testEnv
              $
              -- get_age postgres function returns the age of a user calculated from the
              -- birth_year column and in_year input parameter. The in_year should be a future year
              -- from 2022 (the year when this test is being written)
              [i|
               CREATE OR REPLACE FUNCTION #{ get_age }(user_row "user", in_year integer)
                RETURNS integer
                LANGUAGE plpgsql
                STABLE
               AS $function$
               declare
                 result int;
               begin
               if in_year < 2022 then
                 raise exception 'in_year cannot be less than 2022' using errcode = 22023;
               end if;
               select in_year - user_row.birth_year into result;
               return result;
               end;
               $function$
             |]
            Postgres.run_ testEnv
              $
              -- fetch_users postgres function returns the list of users whose age is equal to given "age" input parameter
              -- in given future "in_year" parameter. The in_year should be a future year
              -- from 2022 (the year when this test is being written) and "age" should not be a negative value.
              [i|
               CREATE OR REPLACE FUNCTION #{ fetch_users }(age integer, in_year integer)
                RETURNS SETOF "user"
                LANGUAGE plpgsql
                STABLE
               AS $function$
               begin
               if (age < 0) or (in_year < 2022) then
                 raise exception 'age cannot be negative or in_year cannot be less than 2022' using errcode=22023;
               end if;
               return query select t.* from "user" t where (in_year - t.birth_year) = age;
               end;
               $function$
             |],
          Fixture.teardownAction = \_ -> pure ()
        }

-- ** Tests

tests :: SpecWith TestEnvironment
tests = do
  -- Test subscriptions with two websocket clients. The database state for the following tests are shared.
  -- Tests involving computed fields
  withSubscriptions (withSubscriptions multiplexedQueryComputedFieldsSpec)
  -- Tests involving custom functions
  withSubscriptions (withSubscriptions multiplexedQueryCustomFunctionsSpec)

multiplexedQueryComputedFieldsSpec ::
  SpecWith (Value -> [Pair] -> IO SubscriptionHandle, (Value -> [Pair] -> IO SubscriptionHandle, TestEnvironment))
multiplexedQueryComputedFieldsSpec = do
  it "Multiplex query in subscriptions - computed fields" \(mkSubscriptionClient2, (mkSubscriptionClient1, testEnv)) -> do
    -- Make two identical graphql subscriptions with a valid and invalid query variable values.
    -- The invalid query should fail and shouldn't affect subscription with valid query.
    let schemaName = Schema.getSchemaName testEnv
        graphqlQuery =
          [graphql|
             subscription($year: Int) {
               #{schemaName}_user{
                 id
                 name
                 get_future_age(args: {in_year: $year})
               }
             }
          |]

        mkVariables :: Int -> [Pair]
        mkVariables year = ["variables" .= object [("year", toJSON year)]]

    -- make subscription with valid future year, 2050.
    subValid <- mkSubscriptionClient1 graphqlQuery $ mkVariables 2050

    -- check result from valid subscription
    shouldReturnYaml
      testEnv
      (getNextResponse subValid)
      [interpolateYaml|
        data:
          #{schemaName}_user:
          - id: 1
            name: user_1
            get_future_age: 54
          - id: 2
            name: user_2
            get_future_age: 50
       |]

    -- make subscription with past year, the query should result in an exception
    subInvalid <- mkSubscriptionClient2 graphqlQuery $ mkVariables 2000

    -- check result from invalid subscription
    shouldReturnYaml
      testEnv
      (getNextResponse subInvalid)
      [yaml|
        errors:
        - extensions:
            code: unexpected
            path: "$"
          message: database query error
       |]

    -- insert a row in the table and check the next response from valid subscription
    let insertRow :: IO Value
        insertRow =
          postGraphql
            testEnv
            [graphql|
              mutation {
                insert_#{schemaName}_user(
                  objects: [{ id: 3, name: "user_3", birth_year: 2005 }]
                ) {
                  affected_rows
                }
              }
            |]

    shouldReturnYaml
      testEnv
      insertRow
      [interpolateYaml|
        data:
          insert_#{schemaName}_user:
            affected_rows: 1
      |]

    shouldReturnYaml
      testEnv
      (getNextResponse subValid)
      [interpolateYaml|
        data:
          #{schemaName}_user:
          - id: 1
            name: user_1
            get_future_age: 54
          - id: 2
            name: user_2
            get_future_age: 50
          - id: 3
            name: user_3
            get_future_age: 45
       |]

multiplexedQueryCustomFunctionsSpec ::
  SpecWith (Value -> [Pair] -> IO SubscriptionHandle, (Value -> [Pair] -> IO SubscriptionHandle, TestEnvironment))
multiplexedQueryCustomFunctionsSpec = do
  it "Multiplex query in subscriptions - custom functions" \(mkSubscriptionClient2, (mkSubscriptionClient1, testEnv)) -> do
    -- Make two identical graphql subscriptions with a valid and invalid query variable values.
    -- The invalid query should fail and shouldn't affect subscription with valid query.
    let schemaName = Schema.getSchemaName testEnv
        graphqlQuery =
          [graphql|
             subscription($age: Int, $year: Int) {
               #{schemaName}_fetch_users(args: {age: $age, in_year: $year}){
                 id
                 name
                 get_future_age(args: {in_year: $year})
               }
             }
          |]

        mkVariables :: Int -> Int -> [Pair]
        mkVariables age year = ["variables" .= object [("age", toJSON age), ("year", toJSON year)]]

    -- make subscription with valid future year, 2050 and valid age 50
    subValid <- mkSubscriptionClient1 graphqlQuery $ mkVariables 50 2050

    -- check result from valid subscription
    shouldReturnYaml
      testEnv
      (getNextResponse subValid)
      [interpolateYaml|
        data:
          #{schemaName}_fetch_users:
          - id: 2
            name: user_2
            get_future_age: 50
       |]

    -- make subscription with past year, the query should result in an exception
    subInvalid <- mkSubscriptionClient2 graphqlQuery $ mkVariables 50 2000

    -- check result from invalid subscription
    shouldReturnYaml
      testEnv
      (getNextResponse subInvalid)
      [yaml|
        errors:
        - extensions:
            code: unexpected
            path: "$"
          message: database query error
       |]

    -- insert a row in the table and check the next response from valid subscription
    let insertRow :: IO Value
        insertRow =
          postGraphql
            testEnv
            [graphql|
              mutation {
                insert_#{schemaName}_user(
                  objects: [{ id: 4, name: "user_4", birth_year: 2000 }]
                ) {
                  affected_rows
                }
              }
            |]

    shouldReturnYaml
      testEnv
      insertRow
      [interpolateYaml|
        data:
          insert_#{schemaName}_user:
            affected_rows: 1
      |]

    shouldReturnYaml
      testEnv
      (getNextResponse subValid)
      [interpolateYaml|
        data:
          #{schemaName}_fetch_users:
          - id: 2
            name: user_2
            get_future_age: 50
          - id: 4
            name: user_4
            get_future_age: 50
       |]
