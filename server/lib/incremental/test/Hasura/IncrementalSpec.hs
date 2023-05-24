{-# LANGUAGE Arrows #-}

module Hasura.IncrementalSpec (spec) where

import Control.Arrow.Extended
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as S
import Hasura.Incremental qualified as Inc
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "cache" $ do
    it "skips re-running rules if the input didn’t change" $ do
      let add1 :: (MonadState Integer m) => m ()
          add1 = modify' (+ 1)

          rule = proc (a, b) -> do
            Inc.cache $ arrM (\_ -> add1) -< a
            Inc.cache $ arrM (\_ -> add1 *> add1) -< b

      (result1, state1) <- runStateT (Inc.build rule (False, False)) 0
      state1 `shouldBe` 3
      (result2, state2) <- runStateT (Inc.rebuild result1 (True, False)) 0
      state2 `shouldBe` 1
      (_, state3) <- runStateT (Inc.rebuild result2 (True, True)) 0
      state3 `shouldBe` 2

    it "tracks dependencies within nested uses of cache across multiple executions" do
      let rule ::
            (MonadWriter String m, MonadIO m) =>
            Inc.Rule m (Inc.InvalidationKey, Inc.InvalidationKey) ()
          rule = proc (key1, key2) -> do
            dep1 <- Inc.newDependency -< key2
            (key1, dep1)
              >-
                Inc.cache
                  ( proc (_, dep2) ->
                      dep2
                        >-
                          Inc.cache
                            ( proc dep3 -> do
                                Inc.dependOn -< dep3
                                arrM tell -< "executed"
                            )
                  )
            returnA -< ()

      let key1 = Inc.initialInvalidationKey
          key2 = Inc.invalidate key1

      (result1, log1) <- runWriterT $ Inc.build rule (key1, key1)
      log1 `shouldBe` "executed"

      (result2, log2) <- runWriterT $ Inc.rebuild result1 (key2, key1)
      log2 `shouldBe` ""

      (_, log3) <- runWriterT $ Inc.rebuild result2 (key2, key2)
      log3 `shouldBe` "executed"

  describe "keyed" $ do
    it "preserves incrementalization when entries don’t change" $ do
      let rule ::
            (MonadWriter (S.HashSet (String, Integer)) m, MonadIO m) =>
            Inc.Rule m (HashMap.HashMap String Integer) (HashMap.HashMap String Integer)
          rule = proc m ->
            (|
              Inc.keyed
                ( \k v -> do
                    Inc.cache $ arrM (tell . S.singleton) -< (k, v)
                    returnA -< v * 2
                )
            |)
              m

      (result1, log1) <- runWriterT . Inc.build rule $ HashMap.fromList [("a", 1), ("b", 2)]
      Inc.result result1 `shouldBe` HashMap.fromList [("a", 2), ("b", 4)]
      log1 `shouldBe` S.fromList [("a", 1), ("b", 2)]
      (result2, log2) <- runWriterT . Inc.rebuild result1 $ HashMap.fromList [("a", 1), ("b", 3), ("c", 4)]
      Inc.result result2 `shouldBe` HashMap.fromList [("a", 2), ("b", 6), ("c", 8)]
      log2 `shouldBe` S.fromList [("b", 3), ("c", 4)]
