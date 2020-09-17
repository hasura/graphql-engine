module Hasura.RQL.MetadataSpec (spec) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                as AO

import           Data.Aeson                        (eitherDecodeStrict)
import           Test.Hspec
import           Test.QuickCheck

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Metadata.Generator (genMetadata)
import           Hasura.RQL.Types.Metadata         (Metadata, metadataToOrdJSON)

spec :: Spec
spec = describe "metadataToOrdJSON" $ do
  it "produces JSON that can be parsed by the FromJSON instance for Metadata" $
    withMaxSuccess 20 $
    forAll (resize 3 genMetadata) $ \metadata ->
      let encodedString = encJToBS $ AO.toEncJSON $ metadataToOrdJSON metadata
      in case eitherDecodeStrict @Metadata encodedString of
           Left err -> counterexample err False
           Right _  -> property True
