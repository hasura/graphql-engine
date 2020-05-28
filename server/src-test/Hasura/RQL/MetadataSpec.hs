module Hasura.RQL.MetadataSpec (spec) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                as AO

import           Data.Aeson                        (eitherDecodeStrict)
import           Test.Hspec
import           Test.QuickCheck

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Metadata.Generator (genReplaceMetadata)
import           Hasura.RQL.DDL.Metadata.Types     (ReplaceMetadata, replaceMetadataToOrdJSON)

spec :: Spec
spec = describe "replaceMetadataToOrdJSON" $ do
  it "produces JSON that can be parsed by the FromJSON instance for ReplaceMetadata" $
    withMaxSuccess 20 $
    forAll (resize 3 genReplaceMetadata) $ \metadata ->
      let encodedString = encJToBS $ AO.toEncJSON $ replaceMetadataToOrdJSON metadata
      in case eitherDecodeStrict @ReplaceMetadata encodedString of
           Left err -> counterexample err False
           Right _  -> property True
