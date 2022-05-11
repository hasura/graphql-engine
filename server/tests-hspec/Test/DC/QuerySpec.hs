{-# LANGUAGE QuasiQuotes #-}

-- | Query Tests for Data Connector Backend
module Test.DC.QuerySpec
  ( spec,
  )
where

import Harness.Backend.DataConnector qualified as DataConnector
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.runWithLocalTestEnvironment
    [ Context.Context
        { name = Context.Backend Context.DataConnector,
          mkLocalTestEnvironment = DataConnector.mkLocalTestEnvironment,
          setup = DataConnector.setup,
          teardown = DataConnector.teardown,
          customOptions = Nothing
        }
    ]
    tests

tests :: Context.Options -> SpecWith (TestEnvironment, a)
tests opts = describe "Queries" $ do
  describe "Basic Tests" $ do
    it "works with simple object query" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
              query getAlbum {
                albums(limit: 1) {
                  id
                  title
                }
              }
            |]
        )
        [yaml|
          data:
            albums: 
              - id: 1
                title: For Those About To Rck We Salute You
        |]

    it "works with order_by id" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(limit: 3, order_by: {id: asc}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums: 
    - id: 1
      title: For Those About To Rck We Salute You
    - id: 2
      title: Balls to the Wall
    - id: 3
      title: Restless and Wild
|]

    it "works with a primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums_by_pk(id: 1) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums_by_pk: 
    - id: 1
      title: "For Those About To Rck We Salute You"
|]

    it "works with non existent primary key" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums_by_pk(id: 999999) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums_by_pk: []
|]

  it "works with pagination" $ \(testEnvironment, _) ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query getAlbum {
  albums (limit: 3, offset: 2) {
    id
  }
}
|]
      )
      [yaml|
data:
  albums:
    - id: 3
    - id: 4
    - id: 5
|]

  describe "Object Relationships" $ do
    it "joins on artist id" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums_by_pk(id: 1) {
    id
    title
    artist {
      name
    }
  }
}
|]
        )
        [yaml|
data:
  albums_by_pk: 
    - id: 1
      title: "For Those About To Rck We Salute You"
      artist:
        - name: "AC/DC"
|]

  describe "Where Clause Tests" $ do
    it "works with '_in' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(where: {id: {_in: [1, 3, 5]}}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums:
  - id: 1
    title: For Those About To Rck We Salute You
  - id: 3
    title: Restless and Wild
  - id: 5
    title: Big Ones
|]

    it "works with '_nin' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(where: {id: {_in: [1, 3, 5]}, title: {_nin: ["Big Ones"]}}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums:
  - id: 1
    title: For Those About To Rck We Salute You
  - id: 3
    title: Restless and Wild
|]

    it "works with '_eq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(where: {id: {_eq: 1}}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums: 
    - id: 1
      title: For Those About To Rck We Salute You
|]

    it "works with '_neq' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(where: {id: {_neq: 2, _in: [1, 2, 3]}}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums: 
    - id: 1
      title: For Those About To Rck We Salute You
    - id: 3
      title: Restless and Wild
|]

    it "works with '_lt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getAlbum {
  albums(where: {id: {_lt: 2}}) {
    id
    title
  }
}
|]
        )
        [yaml|
data:
  albums: 
    - id: 1
      title: For Those About To Rck We Salute You
|]

    it "works with '_lte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getArtists {
  artists(where: {id: {_lte: 2}}) {
    id
    name
  }
}
|]
        )
        [yaml|
data:
  artists: 
    - id: 1
      name: AC/DC
    - id: 2
      name: Accept
|]

    it "works with '_gt' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getArtists {
  artists(where: {id: {_gt: 274}}) {
    id
    name
  }
}
|]
        )
        [yaml|
data:
  artists: 
    - id: 275
      name: Philip Glass Ensemble
|]

    it "works with '_gte' predicate" $ \(testEnvironment, _) ->
      shouldReturnYaml
        opts
        ( GraphqlEngine.postGraphql
            testEnvironment
            [graphql|
query getArtists {
  artists(where: {id: {_gte: 274}}) {
    id
    name
  }
}
|]
        )
        [yaml|
data:
  artists: 
    - id: 274
      name: Nash Ensemble
    - id: 275
      name: Philip Glass Ensemble
|]
