module Data.Parser.TemplateSpec (spec) where

import Data.URL.Template
import Hasura.Prelude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "parseTemplate"
  $ it "template parser and printer"
  $ withMaxSuccess 1000
  $ forAll (arbitrary :: Gen Template)
  $ \template -> do
    let templateString = printTemplate template
    case parseTemplate templateString of
      Left e -> counterexample e False
      Right r -> property $ printTemplate r == templateString
