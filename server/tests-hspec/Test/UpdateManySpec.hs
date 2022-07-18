{-# LANGUAGE QuasiQuotes #-}

-- | Test multiple updates (update_table_many).
module Test.UpdateManySpec (spec) where

import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  describe "UpdateManySpec" $ Context.run [postgresContext] tests
  where
    postgresContext =
      Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Postgres.setup schema,
          teardown = Postgres.teardown schema,
          customOptions = Nothing
        }

--------------------------------------------------------------------------------

-- ** Schema

schema :: [Schema.Table]
schema =
  [ artist,
    album
  ]

artist :: Schema.Table
artist =
  (table "artist")
    { tableColumns =
        [ Schema.column "id" Schema.defaultSerialType,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "first"],
          [Schema.VInt 2, Schema.VStr "second"],
          [Schema.VInt 3, Schema.VStr "third"]
        ]
    }

album :: Schema.Table
album =
  (table "album")
    { tableColumns =
        [ Schema.column "id" Schema.defaultSerialType,
          Schema.column "albumname" Schema.TStr,
          Schema.column "artist_id" Schema.TInt
        ],
      tablePrimaryKey = ["albumname"],
      tableReferences = [Schema.Reference "artist_id" "artist" "id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "first album", Schema.VInt 1],
          [Schema.VInt 2, Schema.VStr "second album", Schema.VInt 2],
          [Schema.VInt 3, Schema.VStr "third album", Schema.VInt 3]
        ]
    }

--------------------------------------------------------------------------------

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Update no records" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  update_hasura_artist_many(
    updates: [
      { where: { id: { _eq: 10 } }
        _set: { name: "test" }
      }
    ]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
data:
  update_hasura_artist_many:
  - affected_rows: 0
|]

  it "Update single record" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  update_hasura_artist_many(
    updates: [
      { where: { id: { _eq: 10 } }
        _set: { name: "test" }
      }
      { where: { id: { _gt: 2 } }
        _set: { name: "test" }
      }
    ]
  ){
    affected_rows
  }
}
|]
      )
      [yaml|
data:
  update_hasura_artist_many:
  - affected_rows: 0
  - affected_rows: 1
|]

  it "Update record multiple times" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  update_hasura_artist_many(
    updates: [
      { where: { id: { _gt: 2 } }
        _set: { name: "test" }
      }
      { where: { name: { _eq: "test" } }
        _set: { name: "changed name" }
      }
    ]
  ){
    affected_rows
    returning {
      name
    }
  }
}
|]
      )
      [yaml|
data:
  update_hasura_artist_many:
  - affected_rows: 1
    returning:
    - name: "test"
  - affected_rows: 1
    returning:
    - name: "changed name"
|]

  it "Update record multiple times with overlapping conditions" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  update_hasura_artist_many(
    updates: [
      { where: { id: { _gt: 2 } }
        _set: { name: "test" }
      }
      { where: { id: { _eq: 3 } }
        _set: { name: "changed name" }
      }
    ]
  ){
    affected_rows
    returning {
      name
    }
  }
}
|]
      )
      [yaml|
data:
  update_hasura_artist_many:
  - affected_rows: 1
    returning:
    - name: "test"
  - affected_rows: 1
    returning:
    - name: "changed name"
|]

  it "Revert on error" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  update_hasura_album_many(
    updates: [
      { where: { id: { _eq: 1 } }
        _set: { albumname: "test" }
      }
      { where: { id: { _eq: 1 } }
        _set: { artist_id: 4 }
      }
    ]
  ){
    affected_rows
    returning {
      name
    }
  }
}
|]
      )
      [yaml|
errors:
  - extensions:
      code: validation-failed
      path: $.selectionSet.update_hasura_album_many.selectionSet.returning.selectionSet.name
    message: 'field "name" not found in type: ''hasura_album'''
|]
