module Main (main) where

import HelloWorldSpec qualified
import SanityCheckSpec qualified
import Test.Hspec
import Prelude

main :: IO ()
main =
  hspec $ do
    describe "SanityCheck" SanityCheckSpec.spec
    describe "HelloWorld" HelloWorldSpec.spec
