{-# LANGUAGE QuasiQuotes #-}

-- | Tests webhook query transformation functionality.
module Test.Webhook.WebhookTransformSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) -> []
            }
        ]
    )
    queryParameterTransformationTests

--------------------------------------------------------------------------------
-- Tests

queryParameterTransformationTests :: Fixture.Options -> SpecWith TestEnvironment
queryParameterTransformationTests opts = do
  describe "Query parameter string generation" do
    it "flatten list of arguments to create & separated query string" \testEnvironment -> do
      let baseUrlTemplate = "{{ $base_url }}"
          queryParamTemplate = "{{ concat ([concat({{ range _, x := $body.input }} \"tags={{x}}&\" {{ end }}), \"flag=smthng\"]) }}"
          expectedWebhookUrl = "http://localhost:3000?tags=apple&tags=banana&flag=smthng"
      verifyTransformation testEnvironment opts baseUrlTemplate queryParamTemplate expectedWebhookUrl

    it "flatten list of arguments to generate comma separated query string" \testEnvironment -> do
      let baseUrlTemplate = "{{ $base_url }}"
          queryParamTemplate = "{{ concat ([\"tags=\", concat({{ range _, x := $body.input }} \"{{x}},\" {{ end }})]) }}"
          expectedWebhookUrl = "http://localhost:3000?tags=apple%2Cbanana%2C"

      verifyTransformation testEnvironment opts baseUrlTemplate queryParamTemplate expectedWebhookUrl

verifyTransformation :: TestEnvironment -> Fixture.Options -> Text -> Text -> Text -> IO ()
verifyTransformation testEnvironment opts baseUrlTemplate queryParamTemplate expectedWebhookUrl = do
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
    opts
    (GraphqlEngine.postMetadata testEnvironment responseTransformQuery)
    expectedResponse
