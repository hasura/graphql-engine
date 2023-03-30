{-# LANGUAGE QuasiQuotes #-}

-- | Tests webhook query transformation functionality.
module Test.Webhook.WebhookTransformSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(_testEnvironment, _) -> []
            }
        ]
    )
    queryParameterTransformationTests

--------------------------------------------------------------------------------
-- Tests

queryParameterTransformationTests :: SpecWith TestEnvironment
queryParameterTransformationTests = do
  describe "Query parameter string generation" do
    it "flatten list of arguments to create & separated query string" \testEnvironment -> do
      let baseUrlTemplate = "{{ $base_url }}"
          queryParamTemplate = "{{ concat ([concat({{ range _, x := $body.input }} \"tags={{x}}&\" {{ end }}), \"flag=smthng\"]) }}"
          expectedWebhookUrl = "http://localhost:3000?tags=apple&tags=banana&flag=smthng"
      verifyTransformation testEnvironment baseUrlTemplate queryParamTemplate expectedWebhookUrl

    it "flatten list of arguments to generate comma separated query string" \testEnvironment -> do
      let baseUrlTemplate = "{{ $base_url }}"
          queryParamTemplate = "{{ concat ([\"tags=\", concat({{ range _, x := $body.input }} \"{{x}},\" {{ end }})]) }}"
          expectedWebhookUrl = "http://localhost:3000?tags=apple%2Cbanana%2C"

      verifyTransformation testEnvironment baseUrlTemplate queryParamTemplate expectedWebhookUrl

verifyTransformation :: TestEnvironment -> Text -> Text -> Text -> IO ()
verifyTransformation testEnvironment baseUrlTemplate queryParamTemplate expectedWebhookUrl = do
  let responseTransformQuery =
        [yaml|
          type: test_webhook_transform
          args:
            webhook_url: http://localhost:3000
            body:
              input:
                - apple
                - banana
            request_transform:
              version: 2
              url: *baseUrlTemplate
              query_params: *queryParamTemplate
              template_engine: Kriti
      |]

      expectedResponse =
        [yaml|
            body:
              input:
                - apple
                - banana
            headers: []
            method: GET
            webhook_url: *expectedWebhookUrl
        |]

  shouldReturnYaml
    testEnvironment
    (GraphqlEngine.postMetadata testEnvironment responseTransformQuery)
    expectedResponse
