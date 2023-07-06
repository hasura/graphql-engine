module Main where

import qualified Counter
import qualified Export
import qualified State
import qualified Store
import Test.Hspec
import qualified Validation
import qualified VectorMetrics

main :: IO ()
main = hspec $ do
  State.tests
  Store.tests
  Counter.tests
  Export.tests
  Validation.tests
  VectorMetrics.tests
