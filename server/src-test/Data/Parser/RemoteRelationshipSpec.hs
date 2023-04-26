{-# LANGUAGE TemplateHaskell #-}

module Data.Parser.RemoteRelationshipSpec (spec) where

import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text.NonEmpty (nonEmptyText)
import Hasura.Base.Error
import Hasura.Base.Error.TestInstances ()
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.LegacyCatalog (parseLegacyRemoteRelationshipDefinition)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RemoteSchema.Metadata
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec

-- | The following json is encoded into @'Value' data type
-- {
--   "remote_schema": "remote_schema_name",
--   "hasura_fields": [
--     "id"
--   ],
--   "remote_field": {
--     "top_level_field": {
--       "arguments": {
--         "id": "$id"
--       }
--     }
--   }
-- }
validLegacyRemoteRelationshipValue :: Value
validLegacyRemoteRelationshipValue =
  object
    [ "remote_schema" .= String "remote_schema_name",
      "hasura_fields" .= toJSON [String "id"],
      "remote_field" .= remoteField
    ]
  where
    remoteField :: Value
    remoteField =
      object ["top_level_field" .= object ["arguments" .= object ["id" .= String "$id"]]]

-- | The following json is encoded into @'Value' data type
-- {
--   "to_remote_schema": {
--     "remote_schema": "remote_schema_name",
--     "lhs_fields": [
--       "id"
--     ],
--     "remote_field": {
--       "top_level_field": {
--         "arguments": {
--           "id": "$id"
--         }
--       }
--     }
--   }
-- }
validUnifiedRemoteRelationshipValue :: Value
validUnifiedRemoteRelationshipValue =
  object
    [ "to_remote_schema"
        .= object
          [ "remote_schema" .= String "remote_schema_name",
            "lhs_fields" .= toJSON [String "id"],
            "remote_field" .= remoteField
          ]
    ]
  where
    remoteField :: Value
    remoteField =
      object ["top_level_field" .= object ["arguments" .= object ["id" .= String "$id"]]]

toSchemaRelationshipDef :: ToSchemaRelationshipDef
toSchemaRelationshipDef =
  ToSchemaRelationshipDef
    { _trrdRemoteSchema = RemoteSchemaName $$(nonEmptyText "remote_schema_name"),
      _trrdLhsFields = Set.singleton (FieldName "id"),
      _trrdRemoteField = remoteField
    }
  where
    remoteField :: RemoteFields
    remoteField =
      let idName = $$(G.litName "id")
          fieldCall =
            FieldCall
              { fcName = $$(G.litName "top_level_field"),
                fcArguments = RemoteArguments $ HashMap.singleton idName (G.VVariable idName)
              }
       in RemoteFields $ fieldCall NE.:| []

validLegacyRemoteRelationshipDefinition :: RemoteRelationshipDefinition
validLegacyRemoteRelationshipDefinition =
  RelationshipToSchema RRFOldDBToRemoteSchema toSchemaRelationshipDef

validUnifiedRemoteRelationshipDefinition :: RemoteRelationshipDefinition
validUnifiedRemoteRelationshipDefinition =
  RelationshipToSchema RRFUnifiedFormat toSchemaRelationshipDef

spec :: Spec
spec = describe "RemoteRelationshipDefinition" $ do
  it "parseLegacyRemoteRelationshipDefinition" $ do
    let parsedValue = runExcept $ parseLegacyRemoteRelationshipDefinition validLegacyRemoteRelationshipValue
    parsedValue `shouldBe` Right validLegacyRemoteRelationshipDefinition

  it "parseLegacyRemoteRelationshipDefinition exception" $ do
    let parsedValue = runExcept $ parseLegacyRemoteRelationshipDefinition validUnifiedRemoteRelationshipValue
    parsedValue `shouldBe` Left (err400 ParseFailed "remote relationship definition (legacy format) expects exactly one of: remote_schema")

  it "parseRemoteRelationshipDefinition Unified" $ do
    let parsedValue = runExcept $ runAesonParser (parseRemoteRelationshipDefinition RRPStrict) validUnifiedRemoteRelationshipValue
    parsedValue `shouldBe` Right validUnifiedRemoteRelationshipDefinition

  it "parseRemoteRelationshipDefinition Unified Exception" $ do
    let parsedValue = runExcept $ runAesonParser (parseRemoteRelationshipDefinition RRPStrict) validLegacyRemoteRelationshipValue
    parsedValue `shouldBe` Left (err400 ParseFailed "remote relationship definition (strict format) expects exactly one of: to_source, to_remote_schema")
