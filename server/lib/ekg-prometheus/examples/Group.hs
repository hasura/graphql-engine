{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HashMap.Strict as HashMap
import GHC.Stats
import System.Metrics.Prometheus

main = do
  store <- newStore
  let metrics =
        [ ("num_gcs", Counter . numGcs),
          ("max_bytes_used", Gauge . maxBytesUsed)
        ]
  registerGroup (HashMap.fromList metrics) getGCStats store
