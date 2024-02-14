{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.BigQuery.SourceSpec (spec) where

import Autodocodec (HasCodec (codec), eitherDecodeJSONViaCodec, object, requiredField', toJSONViaCodec, (.=))
import Data.Aeson (FromJSON, ToJSON (toJSON), Value, eitherDecode, encode)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.TH qualified as J
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Hasura.Backends.BigQuery.Source (ConfigurationInput (FromEnv), ConfigurationJSON (..))
import Hasura.Prelude
import Test.Hspec

-- ConfigurationJSON is used with the real ServiceAccount type - but that type
-- has a private key property that is obnoxious to get a mock value for.
data MockServiceAccount = MockServiceAccount
  { _msaClientEmail :: Text,
    _msaPrivateKey :: Text,
    _msaProjectId :: Text
  }
  deriving (Eq, Show)

instance HasCodec MockServiceAccount where
  codec =
    object "MockServiceAccount"
      $ MockServiceAccount
      <$> requiredField' "client_email"
      .= _msaClientEmail
        <*> requiredField' "private_key"
      .= _msaPrivateKey
        <*> requiredField' "project_id"
      .= _msaProjectId

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = False} ''MockServiceAccount)

spec :: Spec
spec = do
  describe "BigQuery" do
    describe "HasCodec (ConfigurationJSON a)" do
      it "should accept text from a @from_env@ property" do
        let input = encode $ [aesonQQ|{"from_env": "config text"}|]
        input `shouldDecodeTo` FromEnvJSON @(ConfigurationJSON MockServiceAccount) "config text"

      it "should accept data parsed via underlying codec" do
        let clientEmail = "client@test.net"
        let privateKey = "-----BEGIN RSA PRIVATE KEY----- etc."
        let projectId = "2"
        let input =
              encode
                $ [aesonQQ|
                  { client_email: #{clientEmail},
                    private_key: #{privateKey},
                    project_id: #{projectId}
                  }
                |]
        input
          `shouldDecodeTo` FromYamlJSON
            ( MockServiceAccount clientEmail privateKey projectId
            )

      it "should accept a string containing a serialized object with a @from_env@ property" do
        let input = "{\"from_env\":\"config text\"}"
        input `shouldDecodeTo` FromEnvJSON @(ConfigurationJSON MockServiceAccount) "config text"

      it "should accept a string containing a serialized value parsed via underlying codec" do
        let clientEmail = "client@test.net"
        let privateKey = "-----BEGIN RSA PRIVATE KEY----- etc."
        let projectId = "2"
        let input =
              decodeUtf8
                $ encode
                $ [aesonQQ|
                    { client_email: #{clientEmail},
                      private_key: #{privateKey},
                      project_id: #{projectId}
                    }
                  |]
        let stringInput = encode input
        stringInput
          `shouldDecodeTo` FromYamlJSON
            ( MockServiceAccount clientEmail privateKey projectId
            )

      it "should encode to an object with a @from_env@ property" do
        let output = FromEnv "config text"
        output `shouldEncodeTo` [aesonQQ|{from_env: "config text"}|]

      it "should encode via the underlying codec" do
        let clientEmail = "client@test.net"
        let privateKey = "-----BEGIN RSA PRIVATE KEY----- etc."
        let projectId = "2"
        let output = FromYamlJSON $ MockServiceAccount clientEmail privateKey projectId
        output
          `shouldEncodeTo` [aesonQQ|
            { client_email: #{clientEmail},
              private_key: #{privateKey},
              project_id: #{projectId}
            }
          |]

-- | Assert that the given bytestring decodes to the expected value when
-- decoding via Autodocodec, and that the decoded value matches decoding via a
-- FromJSON instance.
shouldDecodeTo ::
  forall a.
  (Eq a, FromJSON a, HasCodec a, Show a) =>
  ByteString ->
  a ->
  Expectation
input `shouldDecodeTo` expected = do
  decoded `shouldBe` (Right expected)
  decoded `shouldBe` decodedViaFromJSON
  where
    decoded = eitherDecodeJSONViaCodec @a input
    decodedViaFromJSON = eitherDecode @a input

-- | Assert that the given value encodes to the expected JSON value when
-- encoding via Autodocodec, and that the encoded value matches encoding via
-- a ToJSON instance.
shouldEncodeTo :: forall a. (HasCodec a, ToJSON a) => a -> Value -> Expectation
output `shouldEncodeTo` expected = do
  encoded `shouldBe` expected
  encoded `shouldBe` encodedViaToJSON
  where
    encoded = toJSONViaCodec output
    encodedViaToJSON = toJSON output
