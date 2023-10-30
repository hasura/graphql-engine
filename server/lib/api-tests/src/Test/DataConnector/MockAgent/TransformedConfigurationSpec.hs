{-# LANGUAGE QuasiQuotes #-}

module Test.DataConnector.MockAgent.TransformedConfigurationSpec (spec) where

import Control.Exception.Safe
import Control.Lens ((?~))
import Data.Aeson (Value)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Has (Has)
import Data.Text.Encoding qualified as Text
import Harness.Backend.DataConnector.Mock (AgentRequest (..), MockAgentEnvironment, MockRequestResults (..), agentConfig, mockAgentGraphqlTest, mockQueryResponse, withMockAgent)
import Harness.Backend.DataConnector.Mock qualified as Mock
import Harness.GraphqlEngine (postMetadataWithStatusAndReqHeaders)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Services.Composed
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (GlobalTestEnvironment)
import Harness.Yaml
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import System.Directory qualified as Directory
import System.FilePath ((</>))
import Test.DataConnector.MockAgent.TestHelpers
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec = do
  secretsPathPrefix <- runIO getSecretsPathPrefix
  withHge
    ( emptyHgeConfig
        { hgeConfigEnvironmentVars =
            [ ("HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX", secretsPathPrefix)
            ]
        }
    )
    $ withMockAgent
    $ withMetadata secretsPathPrefix
    $ tests secretsPathPrefix

getSecretsPathPrefix :: IO FilePath
getSecretsPathPrefix = do
  tempDir <- Directory.getTemporaryDirectory
  pure $ tempDir </> secretsDir

secretsDir :: String
secretsDir = "api-tests-secrets"

secretFile :: String
secretFile = "test_secret.txt"

secretText :: Text
secretText = "I was read from a file"

withMetadata ::
  (Has PostMetadata testEnvironment) =>
  FilePath ->
  SpecWith (testEnvironment, MockAgentEnvironment) ->
  SpecWith (testEnvironment, MockAgentEnvironment)
withMetadata secretsPathPrefix specs =
  flip aroundWith specs \action (testEnvironment, agentEnv) -> do
    Directory.createDirectoryIfMissing False secretsPathPrefix
    BS.writeFile (secretsPathPrefix </> secretFile) (Text.encodeUtf8 secretText)

    let sourceMetadata = mkSourceMetadata secretsPathPrefix
    _ <-
      postMetadataWithStatusAndReqHeaders
        testEnvironment
        200
        []
        [interpolateYaml|
          type: replace_metadata
          args:
            metadata:
              version: 3
              sources:
                - #{sourceMetadata}
              backend_configs: #{agentConfig}
        |]

    action (testEnvironment, agentEnv)
      `finally` Directory.removeDirectoryRecursive secretsPathPrefix

mkSourceMetadata :: FilePath -> Value
mkSourceMetadata secretsPathPrefix =
  let source = BackendType.backendSourceName Mock.backendTypeMetadata
      backendType = BackendType.backendTypeString Mock.backendTypeMetadata
   in [interpolateYaml|
        name : #{source}
        kind: #{backendType}
        tables:
          - table: [Album]
        configuration:
          value: {}
          template: |
            {
              "DEBUG": {
                "session": {{ $session?.foo ?? "foo session default" }},
                "env":     {{ $env?.bar     ?? "bar env default"     }},
                "config":  {{ $config?.baz  ?? "baz config default"  }},
                "vars":    {{ $vars.test_from_file }}
              }
            }
          template_variables:
            test_from_file:
              type: dynamic_from_file
              filepath: #{secretsPathPrefix}/#{secretFile}
      |]

tests ::
  forall testEnvironment.
  (Has PostGraphql testEnvironment) =>
  FilePath ->
  SpecWith (testEnvironment, MockAgentEnvironment)
tests secretsPathPrefix = do
  mockAgentGraphqlTest "works with configuration transformation Kriti template" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              Album(limit: 1) {
                AlbumId
                Title
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 1),
                ("Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    _mrrResponse
      `shouldBeYaml` [yaml|
        data:
          Album:
            - AlbumId: 1
              Title: For Those About To Rock We Salute You
      |]

    _mrrRecordedRequest
      `shouldBe` Just
        ( Query
            $ mkTableRequest
              (mkTableName "Album")
              ( emptyQuery
                  & API.qFields
                  ?~ mkFieldsMap
                    [ ("AlbumId", API.ColumnField (API.ColumnName "AlbumId") (API.ScalarType "number") Nothing),
                      ("Title", API.ColumnField (API.ColumnName "Title") (API.ScalarType "string") Nothing)
                    ]
                    & API.qLimit
                  ?~ 1
              )
        )

    J.toJSON _mrrRecordedRequestConfig
      `shouldBeYaml` [interpolateYaml|
          DEBUG:
            config: "baz config default"
            env: "bar env default"
            session: "foo session default"
            vars: #{secretText}
        |]

  mockAgentGraphqlTest "template variables from files are refreshed for every request" $ \_testEnv performGraphqlRequest -> do
    let headers = []
    let graphqlRequest =
          [graphql|
            query getAlbum {
              Album(limit: 1) {
                AlbumId
                Title
              }
            }
          |]
    let queryResponse =
          mkRowsQueryResponse
            [ [ ("AlbumId", API.mkColumnFieldValue $ J.Number 1),
                ("Title", API.mkColumnFieldValue $ J.String "For Those About To Rock We Salute You")
              ]
            ]
    let mockConfig = mockQueryResponse queryResponse

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    J.toJSON _mrrRecordedRequestConfig
      `shouldBeYaml` [interpolateYaml|
          DEBUG:
            config: "baz config default"
            env: "bar env default"
            session: "foo session default"
            vars: #{secretText}
        |]

    -- Change the secret file text and try again
    let newSecretText = "this has changed!"
    BS.writeFile (secretsPathPrefix </> secretFile) (Text.encodeUtf8 newSecretText)

    MockRequestResults {..} <- performGraphqlRequest mockConfig headers graphqlRequest

    J.toJSON _mrrRecordedRequestConfig
      `shouldBeYaml` [interpolateYaml|
          DEBUG:
            config: "baz config default"
            env: "bar env default"
            session: "foo session default"
            vars: #{newSecretText}
        |]
