-- | A simple implementation of /incremental build rules/, which can be used to avoid unnecessary
-- recomputation on incrementally-changing input. See 'Rule' for more details.
module Hasura.Incremental (
  -- * The @Rule@ datatype
    Rule
  , Result
  , build
  , rebuild
  , rebuildRule
  , result

  -- * Abstract interface
  , ArrowDistribute(..)
  , ArrowCache(..)
  , MonadDepend(..)
  , DependT

  -- * Fine-grained dependencies
  , Dependency
  , Select(Selector)
  , selectD
  , selectKeyD
  , Cacheable(..)
  , Accesses

  -- * Cache invalidation
  , InvalidationKey
  , initialInvalidationKey
  , invalidate
  ) where

import           Hasura.Prelude

import           Hasura.Incremental.Internal.Cache
import           Hasura.Incremental.Internal.Dependency
import           Hasura.Incremental.Internal.Rule
import           Hasura.Incremental.Select

-- | A simple helper type that can be used to implement explicit cache invalidation. Internally,
-- each 'InvalidationKey' is a counter; 'initialInvalidationKey' starts the counter at 0 and
-- 'invalidate' increments it by 1. Two 'InvalidationKey's are equal iff they have the same internal
-- count, so depending on an 'InvalidationKey' provides a mechanism to force portions of the build
-- process to be reexecuted by calling 'invalidate' before running the build.
newtype InvalidationKey = InvalidationKey Int
  deriving (Show, Eq, Cacheable)

initialInvalidationKey :: InvalidationKey
initialInvalidationKey = InvalidationKey 0

invalidate :: InvalidationKey -> InvalidationKey
invalidate (InvalidationKey n) = InvalidationKey (n + 1)
