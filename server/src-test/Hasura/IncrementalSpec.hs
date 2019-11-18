{-# LANGUAGE Arrows #-}

module Hasura.IncrementalSpec (spec) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S

import           Control.Monad.Writer.Strict
import           Test.Hspec

import qualified Hasura.Incremental          as Inc

spec :: Spec
spec = do
  describe "cache" $ do
    it "skips re-running rules if the input didn’t change" $ do
      let add1 :: MonadState Integer m => m ()
          add1 = modify' (+1)

          rule = proc (a, b) -> do
            Inc.cache $ Inc.rule (\_ -> add1) -< a
            Inc.cache $ Inc.rule (\_ -> add1 *> add1) -< b

      let (result1, state1) = runState (Inc.build rule (False, False)) 0
      state1 `shouldBe` 3
      let (result2, state2) = runState (Inc.rebuild result1 (True, False)) 0
      state2 `shouldBe` 1
      let (_, state3) = runState (Inc.rebuild result2 (True, True)) 0
      state3 `shouldBe` 2

  describe "keyed" $ do
    it "preserves incrementalization when entries don’t change" $ do
      let rule :: MonadWriter (S.HashSet (String, Integer)) m
               => Inc.Rule m (M.HashMap String Integer) (M.HashMap String Integer)
          rule = Inc.keyed . Inc.cache . Inc.rule $ \(k, v) ->
            tell (S.singleton (k, v)) $> (v * 2)

      let (result1, log1) = runWriter . Inc.build rule $ M.fromList [("a", 1), ("b", 2)]
      Inc.result result1 `shouldBe` M.fromList [("a", 2), ("b", 4)]
      log1 `shouldBe` S.fromList [("a", 1), ("b", 2)]
      let (result2, log2) = runWriter . Inc.rebuild result1 $ M.fromList [("a", 1), ("b", 3), ("c", 4)]
      Inc.result result2 `shouldBe` M.fromList [("a", 2), ("b", 6), ("c", 8)]
      log2 `shouldBe` S.fromList [("b", 3), ("c", 4)]
