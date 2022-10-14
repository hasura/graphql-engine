{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Metadata.DTO.MetadataDTOSpec (spec) where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value,
    eitherDecode,
    eitherDecodeFileStrict',
  )
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft, isRight)
import Data.Either.Combinators (fromRight')
import Data.FileEmbed (makeRelativeToProject, strToExp)
import Hasura.Metadata.DTO.Metadata (MetadataDTO (..))
import Hasura.Metadata.DTO.MetadataV1 (MetadataV1 (..))
import Hasura.Metadata.DTO.MetadataV2 (MetadataV2 (..))
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3 (..))
import Hasura.Metadata.DTO.Placeholder (PlaceholderArray (PlaceholderArray))
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (Metadata, metadataToDTO)
import Test.Hspec
import Test.Hspec.Expectations.Json (shouldBeJson)

spec :: Spec
spec = describe "MetadataDTO" $ do
  describe "distinguishing metadata revisions" $ do
    it "serializes v1" $ do
      let output = V1 $ emptyMetadataV1
      let expected = [aesonQQ|{ "version": 1, "tables": [] }|]
      toJSON output `shouldBe` expected

    it "serializes v2" $ do
      let output = V2 $ emptyMetadataV2
      let expected = [aesonQQ|{ "version": 2, "tables": [] }|]
      toJSON output `shouldBe` expected

    it "serializes v3" $ do
      let output = V3 $ emptyMetadataV3
      let expected = [aesonQQ|{ "version": 3, "sources": [] }|]
      toJSON output `shouldBe` expected

    it "parses v2" $ do
      let input = "{ \"version\": 2, \"tables\": [] }"
      let expected = V2 $ emptyMetadataV2
      let actual = eitherDecode input :: Either String MetadataDTO
      actual `shouldBe` Right expected

    it "parses v3" $ do
      let input = "{\"version\": 3, \"sources\": [] }"
      let expected = V3 $ emptyMetadataV3
      let actual = eitherDecode input :: Either String MetadataDTO
      actual `shouldBe` Right expected

    it "fails parsing v3 on version mismatch" $ do
      let input = "{\"version\": 3, \"tables\": [] }"
      let actual = eitherDecode input :: Either String MetadataDTO
      actual `shouldSatisfy` isLeft

    it "assumes v1 if version field is absent" $ do
      let input = "{\"tables\": [] }"
      let expected = V1 $ emptyMetadataV1
      let actual = eitherDecode input :: Either String MetadataDTO
      actual `shouldBe` Right expected

    it "fails parsing if input is not v1-compatible, and version is absent" $ do
      let input = "{\"sources\": [] }"
      let actual = eitherDecode input :: Either String MetadataDTO
      actual `shouldSatisfy` isLeft

    beforeAll getMetadataFixture $ do
      describe "v3" $ do
        it "deserializes and re-serializes equivalently to Metadata" $ \(MetadataFixture {..}) -> do
          let dto = parseEither (parseJSON @MetadataDTO) _mfJSON
          let fromDto = toJSON <$> dto
          fromDto `shouldSatisfy` isRight
          (fromRight' fromDto) `shouldBeJson` _mfJSON

        it "converts metadata to DTO to JSON to metadata" $ \(MetadataFixture {..}) -> do
          let dto = metadataToDTO $ _mfMetadata
          let json = toJSON dto
          let metadata = parseEither (parseJSON @Metadata) json
          metadata `shouldBe` (Right _mfMetadata)

emptyMetadataV3 :: MetadataV3
emptyMetadataV3 =
  MetadataV3
    { metaV3Sources = mempty,
      metaV3RemoteSchemas = Nothing,
      metaV3QueryCollections = Nothing,
      metaV3Allowlist = Nothing,
      metaV3Actions = Nothing,
      metaV3CustomTypes = Nothing,
      metaV3CronTriggers = Nothing,
      metaV3RestEndpoints = Nothing,
      metaV3ApiLimits = Nothing,
      metaV3MetricsConfig = Nothing,
      metaV3InheritedRoles = Nothing,
      metaV3GraphqlSchemaIntrospection = Nothing,
      metaV3Network = Nothing,
      metaV3BackendConfigs = Nothing
    }

emptyMetadataV2 :: MetadataV2
emptyMetadataV2 =
  MetadataV2
    { metaV2Actions = Nothing,
      metaV2Allowlist = Nothing,
      metaV2CronTriggers = Nothing,
      metaV2CustomTypes = Nothing,
      metaV2Functions = Nothing,
      metaV2QueryCollections = Nothing,
      metaV2RemoteSchemas = Nothing,
      metaV2Tables = PlaceholderArray mempty
    }

emptyMetadataV1 :: MetadataV1
emptyMetadataV1 =
  MetadataV1
    { metaV1Functions = Nothing,
      metaV1RemoteSchemas = Nothing,
      metaV1Tables = PlaceholderArray mempty
    }

data MetadataFixture = MetadataFixture {_mfMetadata :: Metadata, _mfJSON :: Value}

getMetadataFixture :: IO MetadataFixture
getMetadataFixture = do
  let filePath = $(strToExp =<< makeRelativeToProject "../cli/internal/metadatautil/testdata/json/t2/metadata.json")
  -- Instead of returning loaded JSON as-is, run it through Metadata parsing so
  -- that its format is up-to-date with the current state of the Metadata
  -- structure.
  json <- eitherDecodeFileStrict' @Value filePath
  let metadata = parseEither (parseJSON @Metadata) =<< json
  return $ fromRight' $ MetadataFixture <$> metadata <*> json
