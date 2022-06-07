{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests of the Postgres-specific upsert feature.
module Test.InsertOnConflictSpec (spec) where

import Data.Aeson.Types (Parser, Value, listParser, parseEither, withObject, (.:), (.:?))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Context
import Harness.Test.Permissions (Permission (..))
import Harness.Test.Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import Test.Hspec hiding (context)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = run [postgresContext, citusContext] (\_ -> tests)

postgresContext :: Context ()
postgresContext =
  (context $ Backend BackendType.Postgres)
    { setup = \(t, _) -> do
        Postgres.setup tables (t, ())
        Postgres.setupPermissions (permissions "postgres") t,
      teardown = \(t, _) -> do
        Postgres.teardownPermissions (permissions "postgres") t
        Postgres.teardown tables (t, ())
    }

citusContext :: Context ()
citusContext =
  (context $ Backend BackendType.Citus)
    { setup = \(t, _) -> do
        Citus.setup tables (t, ())
        Citus.setupPermissions (permissions "citus") t,
      teardown = \(t, _) -> do
        Citus.teardownPermissions (permissions "citus") t
        Citus.teardown tables (t, ())
    }

tables :: [Table]
tables =
  [ (table "foo")
      { tableColumns =
          [ column "id" TInt,
            column "bar" TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [VInt 0, VStr "initial"]
          ]
      }
  ]

permissions :: Text -> [Permission]
permissions source =
  [ SelectPermission
      { permissionTable = "foo",
        permissionSource = source,
        permissionRole = "role-select-only",
        permissionColumns = ["id", "bar"]
      },
    InsertPermission
      { permissionTable = "foo",
        permissionSource = source,
        permissionRole = "role-insert-only",
        permissionColumns = ["id", "bar"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests =
  -- Tests relating to https://github.com/hasura/graphql-engine/issues/8260
  describe "The schema for insert mutations with an 'on_conflict' clause" do
    describe "When no columns are updateable" do
      it "Is still present with an empty enum" testEmptyUpdateColumns
      it "Inserts ignoring duplicates" testInsertDoNothing

testEmptyUpdateColumns :: TestEnvironment -> IO ()
testEmptyUpdateColumns env = do
  introspectTypes env "role-insert-only"
    >>= (`shouldContain` ["hasura_foo_on_conflict"])

  introspectEnums env "role-insert-only"
    >>= (`shouldContain` [("hasura_foo_update_column", ["_PLACEHOLDER"])])

testInsertDoNothing :: TestEnvironment -> IO ()
testInsertDoNothing env = do
  -- We can insert ignoring duplicates
  GraphqlEngine.postGraphqlWithHeaders
    env
    [("X-Hasura-Role", encodeUtf8 "role-insert-only")]
    [graphql|
        mutation OnConflictDoNothing {
          insert_hasura_foo
          (
            objects: [
              {bar: "untouched", id: 0},
              {bar: "inserted",  id: 1}],
            on_conflict: {constraint: foo_pkey, update_columns: []}
          )
          {
            affected_rows
          }
        }
      |]
    >>= ( `shouldBe`
            [yaml|
              data:
                insert_hasura_foo:
                  affected_rows: 1
            |]
        )

  -- The data actually gets stored
  GraphqlEngine.postGraphqlWithHeaders
    env
    [("X-Hasura-Role", encodeUtf8 "role-select-only")]
    [graphql|
      query ActualData {
        hasura_foo {
          bar
          id
        }
      }
      |]
    >>= ( `shouldBe`
            [yaml|
              data:
                hasura_foo:
                - bar: "initial"
                  id: 0
                - bar: "inserted"
                  id: 1
            |]
        )

----------------------------------
-- Test helpers

introspectTypes :: TestEnvironment -> Text -> IO [Text]
introspectTypes env role = do
  res <-
    GraphqlEngine.postGraphqlWithHeaders
      env
      [("X-Hasura-Role", encodeUtf8 role)]
      [graphql|
          query IntrospectTypes { __schema {
            types { name }
          }}
        |]
  onLeft (parseEither getTypes res) fail
  where
    getTypes :: Value -> Parser [Text]
    getTypes = withObject "introspection top-level" $ \top -> do
      d <- top .: "data"
      sch <- d .: "__schema"
      types <- sch .: "types"
      listParser parseType types

    parseType :: Value -> Parser Text
    parseType = withObject "a 'types' element" $ \types -> types .: "name"

introspectEnums :: TestEnvironment -> Text -> IO [(Text, [Text])]
introspectEnums env role = do
  res <-
    GraphqlEngine.postGraphqlWithHeaders
      env
      [("X-Hasura-Role", encodeUtf8 role)]
      [graphql|
        query IntrospectEnums {
          __schema {
            types {
              enumValues {
                name
              }
              name
            }
          }
        }
      |]
  onLeft (parseEither getEnums res) fail
  where
    getEnums :: Value -> Parser [(Text, [Text])]
    getEnums = withObject "introspection top-level" $ \top -> do
      d <- top .: "data"
      sch <- d .: "__schema"
      types <- sch .: "types"
      catMaybes <$> listParser parseEnum types

    parseEnum :: Value -> Parser (Maybe (Text, [Text]))
    parseEnum = withObject "a 'types' element" $ \types -> do
      name <- types .: "name"
      maybeVals <- types .:? "enumValues"
      case maybeVals of
        Nothing -> return Nothing
        Just vals -> Just . (name,) <$> listParser parseEnumValue vals

    parseEnumValue :: Value -> Parser Text
    parseEnumValue = withObject "enumValue" (.: "name")
