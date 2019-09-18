module Control.Concurrent.Extended
  ( module Control.Concurrent
  , threadDelay
  ) where

import           Prelude

import qualified Control.Concurrent    as Base

import           Control.Concurrent    hiding (threadDelay)
import           Data.Time.Clock       (DiffTime)
import           Data.Time.Clock.Units (Microseconds (..))

-- | Like 'Base.threadDelay', but takes a 'DiffTime' instead of an 'Int'.
threadDelay :: DiffTime -> IO ()
threadDelay = Base.threadDelay . round . Microseconds
