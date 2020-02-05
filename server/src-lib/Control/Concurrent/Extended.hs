module Control.Concurrent.Extended
  ( module Control.Concurrent
  , sleep
  -- * Deprecated
  , threadDelay
  ) where

import           Prelude

import qualified Control.Concurrent    as Base

import           Control.Concurrent    hiding (threadDelay)
import           Data.Time.Clock.Units (Microseconds (..), DiffTime)

-- | Like 'Base.threadDelay', but takes a 'DiffTime' instead of an 'Int' microseconds.
--
-- NOTE: you cannot simply replace e.g. @threadDelay 1000@ with @sleep 1000@ since those literals
-- have different meanings!
sleep :: DiffTime -> IO ()
sleep = Base.threadDelay . round . Microseconds

{-# DEPRECATED threadDelay "Please use `sleep` instead (and read the docs!)" #-}
threadDelay :: Int -> IO ()
threadDelay = Base.threadDelay
