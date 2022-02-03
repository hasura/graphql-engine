{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test directives.
module Test.DirectivesSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Mysql as Mysql
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql
import Harness.Quoter.Yaml
import Harness.State (State)
import Harness.Test.Feature qualified as Feature
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Feature.feature
    Feature.Feature
      { Feature.backends =
          [ Feature.Backend
              { name = "MySQL",
                setup = mysqlSetup,
                teardown = mysqlTeardown
              }
          ],
        Feature.tests = tests
      }

--------------------------------------------------------------------------------
-- MySQL backend

mysqlSetup :: State -> IO ()
mysqlSetup state = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state Mysql.defaultSourceMetadata

  -- Setup tables
  Mysql.run_
    [sql|
CREATE TABLE author
(
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(45) UNIQUE KEY
);
|]
  Mysql.run_
    [sql|
INSERT INTO author
    (name)
VALUES
    ( 'Author 1'),
    ( 'Author 2');
|]

  -- Track the tables
  GraphqlEngine.post_
    state
    "/v1/metadata"
    [yaml|
type: mysql_track_table
args:
  source: mysql
  table:
    schema: hasura
    name: author
|]

mysqlTeardown :: State -> IO ()
mysqlTeardown _ = do
  Mysql.run_
    [sql|
DROP TABLE author;
|]

--------------------------------------------------------------------------------
-- Tests

data QueryParams = QueryParams
  { includeId :: Bool,
    skipId :: Bool
  }

query :: QueryParams -> Value
query QueryParams {includeId, skipId} =
  [graphql|
  query author_with_both {
    hasura_author {
      id @include(if: #{includeId}) @skip(if: #{skipId})
      name
    }
  }
|]

tests :: SpecWith State
tests = do
  it "Skip id field conditionally" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          (query QueryParams {includeId = False, skipId = False})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, includeId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          (query QueryParams {includeId = True, skipId = False})
      )
      [yaml|
data:
  hasura_author:
  - id: 1
    name: Author 1
  - id: 2
    name: Author 2
|]

  it "Skip id field conditionally, skipId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          (query QueryParams {includeId = False, skipId = True})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  it "Skip id field conditionally, skipId=true, includeId=true" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
          (query QueryParams {includeId = True, skipId = True})
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]

  -- These two come from <https://github.com/hasura/graphql-engine-mono/blob/ec3568c704c4c3f13ecff757c547f0d5a272307b/server/tests-py/queries/graphql_query/mysql/select_query_author_with_skip_directive.yaml#L1>

  it "Author with skip id" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_skip($skipId: Boolean!, $skipName: Boolean!) {
    hasura_author {
      id @skip(if: $skipId)
      name @skip(if: $skipName)
    }
  }
variables:
  skipId: true
  skipName: false
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
  - name: Author 2
|]
  it "Author with skip name" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
query: |
  query author_with_skip($skipId: Boolean!, $skipName: Boolean!) {
    hasura_author {
      id @skip(if: $skipId)
      name @skip(if: $skipName)
    }
  }
variables:
  skipId: false
  skipName: true
|]
      )
      [yaml|
data:
  hasura_author:
  - id: 1
  - id: 2
|]

  -- These three come from <https://github.com/hasura/graphql-engine-mono/blob/5f6f862e5f6b67d82cfa59568edfc4f08b920375/server/tests-py/queries/graphql_query/mysql/select_query_author_with_wrong_directive_err.yaml#L1>
  it "Rejects unknown directives" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
    query: |
      query {
        hasura_author {
          id @exclude(if: true)
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.hasura_author.selectionSet
    code: validation-failed
  message: directive "exclude" is not defined in the schema
|]
  it "Rejects duplicate directives" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
    query: |
      query {
        hasura_author {
          id @include(if: true) @include(if: true)
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $.selectionSet.hasura_author.selectionSet
    code: validation-failed
  message: 'the following directives are used more than once: include'
|]
  it "Rejects directives on wrong element" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphqlYaml
          state
          [yaml|
    query: |
      query @include(if: true) {
        hasura_author {
          id
          name
        }
      }
|]
      )
      [yaml|
errors:
- extensions:
    path: $
    code: validation-failed
  message: directive "include" is not allowed on a query
|]
