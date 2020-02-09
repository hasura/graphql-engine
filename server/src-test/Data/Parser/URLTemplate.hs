module Data.Parser.URLTemplate (spec) where

import           Hasura.Prelude

import           Data.URL.Template
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "parseURLTemplate" $
  it "URL template parser and printer" $
    withMaxSuccess 1000 $
    forAll genURLTemplate $ \urlTemplate -> do
    let templateString = printURLTemplate urlTemplate
    case parseURLTemplate templateString of
      Left e  -> counterexample e False
      Right r -> property $ printURLTemplate r == templateString
