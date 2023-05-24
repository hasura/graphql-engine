{-# LANGUAGE QuasiQuotes #-}

-- | Tests for customization of type names/field names of remote schemas.
module Test.Schema.RemoteSchemaCustomizationSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Fixture (Fixture (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment, stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment (NE.fromList [context]) tests
  where
    context =
      (Fixture.fixture $ Fixture.RemoteGraphQLServer)
        { -- start only one remote server
          Fixture.mkLocalTestEnvironment = \_testEnvironment ->
            RemoteServer.run
              $ RemoteServer.generateQueryInterpreter
              $ Query
                { echoEnum = echoEnumResolver
                },
          setupTeardown = \(testEnvironment, server) ->
            [ Fixture.SetupAction
                { Fixture.setupAction =
                    addRemoteSchema testEnvironment "remote1" "remote2" server,
                  Fixture.teardownAction = \_ -> do
                    GraphqlEngine.clearMetadata testEnvironment
                    stopServer server
                }
            ]
        }

-- | Add a remote schema to the engine with the given name.
addRemoteSchema :: TestEnvironment -> String -> String -> Server -> IO ()
addRemoteSchema testEnvironment rsName1 rsName2 remoteServer = do
  let remoteSchemaEndpoint = RemoteServer.graphqlEndpoint remoteServer
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:

- type: add_remote_schema
  args:
    name: *rsName1
    definition:
      url: *remoteSchemaEndpoint
      customization:
        type_names:
          prefix: customPrefix_
        field_names:
          - parent_type: Query
            suffix: _customFieldSuffix
- type: add_remote_schema
  args:
    name: *rsName2
    definition:
      url: *remoteSchemaEndpoint
    |]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Server)
tests = do
  describe "Enums with customized remote schema" $ do
    it "works when enums are passed as a variable to an argument to a type name customized remote schema field" \(testEnvironment, _) -> do
      let query =
            [graphql|
              query MyQuery ($x: customPrefix_Profession!) {
                echoEnum_customFieldSuffix (x: $x)
              }
             |]
          variables =
            [yaml|
              x: "ENGINEER"
                 |]
          expectedResponse =
            [yaml|
            data:
              echoEnum_customFieldSuffix: "ENGINEER"
                 |]

      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postGraphqlWithVariables testEnvironment query variables)
        expectedResponse

    it "works when enums are passed an argument to a type name customized remote schema field" \(testEnvironment, _) -> do
      let query =
            [graphql|
              query MyQuery {
                echoEnum (x: DOCTOR)
              }
             |]
          expectedResponse =
            [yaml|
            data:
              echoEnum: "DOCTOR"
                 |]

      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postGraphql testEnvironment query)
        expectedResponse

  describe "Enums with uncustomized remote schema" $ do
    it "works when enums are passed as a variable to an argument" \(testEnvironment, _) -> do
      let query =
            [graphql|
              query MyQuery ($x: Profession!) {
                echoEnum (x: $x)
              }
             |]
          variables =
            [yaml|
              x: "ENGINEER"
                 |]
          expectedResponse =
            [yaml|
            data:
              echoEnum: "ENGINEER"
                 |]

      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postGraphqlWithVariables testEnvironment query variables)
        expectedResponse

    it "works when enums are passed an argument directly" \(testEnvironment, _) -> do
      let query =
            [graphql|
              query MyQuery {
                echoEnum (x: DOCTOR)
              }
             |]
          expectedResponse =
            [yaml|
            data:
              echoEnum: "DOCTOR"
                 |]

      shouldReturnYaml
        testEnvironment
        (GraphqlEngine.postGraphql testEnvironment query)
        expectedResponse

--------------------------------------------------------------------------------
-- Remote schema

[gqlDocument|

type Query {
  echoEnum (x: Profession!): Profession!
}

enum Profession {
  ENGINEER
  DOCTOR
  ARCHITECT
}

|]

echoEnumResolver :: (Monad m) => Arg "x" Profession -> m Profession
echoEnumResolver (Arg x) = pure x
