module Counter
  ( tests,
  )
where

import Tasks (incrementCounterWithMultipleWriters)
import Test.Hspec

tests :: Spec
tests =
  describe "The `Counter` metric" $ do
    it "is thread-safe" test_threads

test_threads :: IO ()
test_threads = do
  result <- incrementCounterWithMultipleWriters
  result `seq` pure ()
