-- | A simple implementation of /incremental build rules/, which can be used to avoid unnecessary
-- recomputation on incrementally-changing input. See 'Rule' for more details.
module Hasura.Incremental
  ( Rule
  , Result
  , build
  , rebuild
  , rebuildRule
  , result

  , ArrowDistribute(..)
  , ArrowCache(..)
  , MonadDepend(..)
  , DependT

  , Dependency
  , Selector
  , selectD
  , selectKeyD
  , Cacheable(..)
  , Accesses
  ) where

import           Hasura.Incremental.Internal.Cache
import           Hasura.Incremental.Internal.Dependency
import           Hasura.Incremental.Internal.Rule
import           Hasura.Incremental.Select
