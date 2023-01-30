{-# LANGUAGE QuasiQuotes #-}

module Hasura.RQL.Types.Relationships.RemoteSpec (spec) where

import Autodocodec (HasCodec, parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parse)
import Hasura.Prelude
import Hasura.RQL.Types.Relationships.Remote (RemoteRelationship)
import Test.Hspec

spec :: Spec
spec = do
  describe "RemoteRelationshipDefinition" do
    it "should serialize a relationship targeting a source" do
      shouldRoundTripEquivalentlyToJSON @RemoteRelationship
        [aesonQQ|
          { name: "relToSource",
            definition: {
              to_source: {
                relationship_type: "object",
                field_mapping: { "foo": "bar" },
                source: "default",
                table: "my_table"
              }
            }
          }
        |]

    it "should serialize a relationship targeting a schema in unified format" do
      shouldRoundTripEquivalentlyToJSON @RemoteRelationship
        [aesonQQ|
          { name: "relToSchema",
            definition: {
              to_remote_schema: {
                remote_schema: "my_remote_schema",
                lhs_fields: ["foo", "bar"],
                remote_field: {
                  foo: {
                    arguments: { "foo_arg_1": "foo_1" },
                    subfields: {
                      bar: {
                        arguments: { "bar_arg_1": "bar_1"}
                      }
                    }
                  }
                }
              }
            }
          }
        |]

    it "should serialize a relationship targeting a schema in legacy format" do
      shouldRoundTripEquivalentlyToJSON @RemoteRelationship
        [aesonQQ|
          { name: "relToSchema",
            definition: {
              remote_schema: "my_remote_schema",
              hasura_fields: ["foo", "bar"],
              remote_field: {
                foo: {
                  arguments: { "foo_arg_1": "foo_1" },
                  subfields: {
                    bar: {
                      arguments: { "bar_arg_1": "bar_1"}
                    }
                  }
                }
              }
            }
          }
        |]

shouldRoundTripEquivalentlyToJSON :: forall a. (Eq a, HasCodec a, FromJSON a, ToJSON a, Show a) => Value -> Expectation
shouldRoundTripEquivalentlyToJSON input = do
  decodedViaCodec `shouldBe` decodedViaJSON
  encodedViaCodec `shouldBe` encodedViaJSON
  where
    decodedViaCodec = parse (parseJSONViaCodec @a) input
    decodedViaJSON = fromJSON @a input
    encodedViaCodec = toJSONViaCodec <$> decodedViaCodec
    encodedViaJSON = toJSON <$> decodedViaJSON
