{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Hasura.Incremental.Internal.Cache
  ( ArrowCache (..),
  )
where

import Control.Arrow.Extended
import Data.Reflection
import Hasura.Incremental.Internal.Dependency
import Hasura.Incremental.Internal.Rule
import Hasura.Incremental.Select
import Hasura.Prelude

class (ArrowKleisli m arr) => ArrowCache m arr | arr -> m where
  -- | Adds equality-based caching to the given arrow. After each execution of the arrow, its input
  -- and result values are cached. On the next execution, the new input value is compared via '=='
  -- to the previous input value. If they are the same, the previous result is returned /without/
  -- re-executing the arrow. Otherwise, the old cached values are discarded, and the arrow is
  -- re-executed to produce a new set of cached values.
  --
  -- Indescriminate use of 'cache' is likely to have little effect except to increase memory usage,
  -- since the input and result of each execution must be retained in memory. Avoid using 'cache'
  -- around arrows with large input or output that is likely to change often unless profiling
  -- indicates it is computationally expensive enough to be worth the memory overhead.
  --
  -- __Note that only direct inputs and outputs of the given arrow are cached.__ If an arrow
  -- provides access to values through a side-channel, they will __not__ participate in caching.
  cache :: ((Given Accesses) => Eq a) => arr a b -> arr a b

  -- | Creates a new 'Dependency', which allows fine-grained caching of composite values; see the
  -- documentation for 'Dependency' for more details.
  newDependency :: arr a (Dependency a)

  -- | Extract the value from a 'Dependency', incurring a dependency on its entirety. To depend on
  -- only a portion of the value, use 'selectD' or 'selectKeyD' before passing it to 'dependOn'.
  dependOn :: (Eq a) => arr (Dependency a) a

instance (ArrowChoice arr, ArrowCache m arr) => ArrowCache m (ErrorA e arr) where
  cache (ErrorA f) = ErrorA (cache f)
  {-# INLINE cache #-}
  newDependency = liftA newDependency
  {-# INLINE newDependency #-}
  dependOn = liftA dependOn
  {-# INLINE dependOn #-}

instance (Monoid w, ArrowCache m arr) => ArrowCache m (WriterA w arr) where
  cache (WriterA f) = WriterA (cache f)
  {-# INLINE cache #-}
  newDependency = liftA newDependency
  {-# INLINE newDependency #-}
  dependOn = liftA dependOn
  {-# INLINE dependOn #-}

instance (MonadIO m) => ArrowCache m (Rule m) where
  cache ::
    forall a b.
    ((Given Accesses) => Eq a) =>
    Rule m a b ->
    Rule m a b
  cache r0 = Rule \s a k -> do
    let Rule r = listenAccesses r0
    r s a \s' (b, accesses) r' -> k s' b (cached accesses a b r')
    where
      listenAccesses :: Rule m a b -> Rule m a (b, Accesses)
      listenAccesses (Rule r) = Rule \s a k -> r mempty a \s' b r' ->
        (k $! (s <> s')) (b, s') (listenAccesses r')

      cached :: Accesses -> a -> b -> Rule m a (b, Accesses) -> Rule m a b
      cached accesses a b (Rule r) = Rule \s a' k ->
        if
          | unchanged accesses a a' -> (k $! (s <> accesses)) b (cached accesses a b (Rule r))
          | otherwise -> r s a' \s' (b', accesses') r' -> k s' b' (cached accesses' a' b' r')

  newDependency = Rule \s a k -> do
    key <- DependencyRoot <$> newUniqueS
    k s (Dependency key a) (arr (Dependency key))
  {-# INLINEABLE newDependency #-}

  dependOn = Rule \s (Dependency key v) k -> (k $! recordAccess key AccessedAll s) v dependOn
