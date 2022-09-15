{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.CircularSpec (spec) where

import Control.Monad.Circular
import Control.Monad.MemoizationSpecDefinition
import Hasura.Prelude
import Test.Hspec

instance Memoizer CircularT where
  runMemoizer = runCircularT
  memoize = const withCircular

spec :: Spec
spec = memoizationSpec @CircularT
