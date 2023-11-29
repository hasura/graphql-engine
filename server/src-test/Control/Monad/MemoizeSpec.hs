{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MemoizeSpec (spec) where

import Control.Monad.MemoizationSpecDefinition
import Control.Monad.Memoize
import Hasura.Prelude
import Test.Hspec

-- We need to add a couple of extra parameters, 'k' and 'v', to align with the
-- shape of a memoizer as understood by 'Memoizer'.
newtype MemoizeWithExtraParamsT k v m a = MemoizeWithExtraParamsT {unMemoizeWithExtraParamsT :: MemoizeT m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance Memoizer MemoizeWithExtraParamsT where
  runMemoizer = runMemoizeT . unMemoizeWithExtraParamsT
  memoize name key = MemoizeWithExtraParamsT . memoizeOn name key . unMemoizeWithExtraParamsT

deriving newtype instance (MonadState s m) => MonadState s (MemoizeWithExtraParamsT k v m)

spec :: Spec
spec = memoizationSpec @MemoizeWithExtraParamsT
