module Control.Concurrent.ExtendedSpec
  ( spec,
  )
where

import Control.Concurrent.Extended qualified as C
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "forConcurrentlyEIO" $ do
    it "chunks correctly" $ do
      for_ [1, 2, 3, 4, 5] $ \chunkSize -> do
        out <- runExceptT $ C.forConcurrentlyEIO chunkSize [pure 1, pure 2, pure 3, pure 4] id
        out `shouldBe` (Right [1, 2, 3, 4] :: Either Int [Int])

    it "behaves like ExceptT" $ do
      for_ [1, 2, 3, 4, 5] $ \chunkSize -> do
        out <- runExceptT $ C.forConcurrentlyEIO chunkSize [pure 1, throwError 2, throwError 3, pure 4] id
        out `shouldBe` (Left 2 :: Either Int [Int])
