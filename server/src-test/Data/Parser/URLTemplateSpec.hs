module Data.Parser.URLTemplateSpec (spec) where

import Data.URL.Template
import Hasura.Prelude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "parseURLTemplate" $
  it "URL template parser and printer" $
    withMaxSuccess 1000 $
      forAll genURLTemplate $ \urlTemplate -> do
        let templateString = printURLTemplate urlTemplate
        case parseURLTemplate templateString of
          Left e -> counterexample e False
          Right r -> property $ printURLTemplate r == templateString
