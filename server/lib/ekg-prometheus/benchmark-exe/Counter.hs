-- | Perform 100,000 atomic increments using 100 concurrent writers.
module Main where

import Criterion
import Criterion.Main (defaultMain)
import Tasks
import Prelude hiding (read)

main :: IO ()
main =
  defaultMain
    [ bench "Increment counter with multiple writers" $
        whnfIO incrementCounterWithMultipleWriters
    ]
