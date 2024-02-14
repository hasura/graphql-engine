module System.Metrics.Prometheus.ThreadId
  ( myCapability,
  )
where

import qualified Control.Concurrent as Concurrent

myCapability :: IO Int
myCapability =
  fst <$> (Concurrent.threadCapability =<< Concurrent.myThreadId)
