{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Concurrent            (getNumCapabilities)
import           Control.Concurrent.Async      (forConcurrently)
import           Control.DeepSeq
import           Control.Monad                 (foldM)
import           Criterion.Main
import           Data.Bits
import           Data.IORef
import           Data.List
import           Data.List.Split               (chunksOf)
import           Data.Ord
import           Data.Traversable
import qualified Data.Vector                   as V
import           Data.Word
import           GHC.Clock
import qualified Hasura.Cache.Bounded          as B
import qualified Hasura.Cache.Unbounded        as U
import           Prelude
import           System.Random.MWC             as Rand
import           System.Random.MWC.Probability as Rand
-- higher level interface to above, combined:
import qualified Hasura.Cache                  as Cache

-- Benchmarks for code backing the plan cache.

main :: IO ()
main = defaultMain [
    -- simple lookup benchmarks at different capacities. Although reads are effectful
    -- in Bounded cache, we don't expect this to cause drift in timings.
    bgroup "lookup" [
      readBenches 1
    , readBenches 100
      -- This is the maximum capacity for bounded at the moment. Make 1mil if
      -- we increase this bound:
    , readBenches 65535
    ]
    -- simple insert benchmark. Try to avoid drift by initialising fresh
    -- and measuring 1000 inserts at a time.
  , env (randomInts 1000) $ \ ~rs->
      bgroup "insert x1000" [
        -- use perRunEnv so we can be sure we're not triggering cache
        -- evictions in bounded due to long bootstrap batch runs
        bench "unbounded" $
          perRunEnv (U.initialise) $ \cache ->
            V.mapM_ (\k -> U.insert k k cache) rs
      , bench "bounded" $
          perRunEnv (B.initialise 4000) $ \cache ->
            V.mapM_ (\k -> B.insert k k cache) rs
      -- an eviction on each insert, all LRU counters at zero. Simulates a scan.
      , bench "bounded evicting scan" $
          let preloaded = populate 5000 (B.initialise 5000) B.insertAllStripes
           in perRunEnv (preloaded) $ \(cache, _) ->
                V.mapM_ (\k -> B.insert k k cache) rs
      ]

  ---- lookup+insert loops on realistic data, with a tunable cost of a cache
  ---- miss.
  --
  -- No extra cost to a cache miss. This might be useful to remove noise or
  -- enhance contention effects:
  , realisticBenches "realistic requests x1000000, no miss cost" 0
  -- Here we simulate generating a plan on a miss. The timing here was obtained
  -- at commit b81d22f58 by measuring the average runtime difference in runGQ
  -- when caching was enabled and disabled, for one particular query.
  --
  -- There are a lot of other valid numbers we could use here though, e.g. 1.5
  -- ms was approximately the minimum cost; the avg was heavily skewed.
  , realisticBenches "realistic requests x1000000, real plan gen cost" 200000 -- ~20ms
  -- A number pulled out of the air, to give us a sense of what optimizing plan
  -- generation might do for us in presence of caching:
  , realisticBenches "realistic requests x1000000, optimized 1ms miss cost" 10000 -- ~1ms

  , bgroup "misc" [
      -- Is burnCycles valid as a tunable consistent workload?
      bench "burnCycles x1" $ nfAppIO burnCycles 1
    , bench "burnCycles x2" $ nfAppIO burnCycles 2
    , bench "burnCycles x4" $ nfAppIO burnCycles 4
    , bench "burnCycles x1000" $ nfAppIO burnCycles 1000
    ]
  ]

-- | Simulate a realistic lookup+workload+insert loop on zipf-distributed data,
-- with a tunable workload to simulate the cost of a cache miss.
--
-- NOTE: our hypothesis (that requests are power law distributed) might not be
-- correct, or might be incorrect for some users. Or it might be that many
-- users interact with hasura ONLY with parameterized queries with variables,
-- where all of these fit into a fairly small cache (but where occurrences of
-- these are zipf-distributed). (TODO It should be simple to adapt this to the latter
-- case (just test on zipf Word8 domain), but these benchmarks don't seem very
-- useful if we assume we effectively get only cache hits).
--
-- This might give us insight into:
--   - Are stripes actually helpful?
--   - Does contention cause issues (e.g. can we induce it w/ wrk=0?)?
--      Do we want a lockfree algorithm?
--   - Is it worthwhile to try to improve performance of plan generation (here
--      simulated by decreasing the 'wrk' parameter)?
--   - Could we benefit from a more efficient lookup/insert loop e.g. by hashing only once?
--   - What might be a good default cache size bound?
--   - Different caching algorithms/schemes:
--     - alternatives to LRU (although we don't intentionally simulate scans here)
--     - caching with random probability to chop long tail
--     - ...
realisticBenches :: String -> Int -> Benchmark
realisticBenches name wrk =
  bgroup name [
    --  27K uniques, 97% in top 10%, 97% cache hits ideally
    env (zipfianRandomInts 1000000 1.4) $ \ ~(payloads, _,_,_,_)->  -- EXPENSIVE!
      bgroup "optimistic distribution" $
        -- For oversubscribed case: can we see descheduled threads blocking global progress?
        flip map [2,100] $ \threadsPerHEC ->
          bgroup (show threadsPerHEC <>"xCPUs threads") [
            bench "unbounded" $
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions Nothing) $ \cache ->
                go threadsPerHEC cache payloads
          , bench "bounded effectively unbounded" $
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions $ Just 40000) $ \cache ->
                go threadsPerHEC cache payloads
          , bench "bounded 10pct ideal capacity" $
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions $ Just 2700) $ \cache ->
                go threadsPerHEC cache payloads
          ]
    --  660K uniques, 40% in top 10% , 30% in top 1%, 33% cache hits ideally
  , env (zipfianRandomInts 1000000 1.01) $ \ ~(payloads, _,_,_,_)->  -- EXPENSIVE!
      bgroup "realistic distribution" $
        flip map [2,100] $ \threadsPerHEC ->
          bgroup (show threadsPerHEC <>"xCPUs threads") [
            bench "unbounded" $
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions Nothing) $ \cache ->
                go threadsPerHEC cache payloads
          , bench "bounded maxBound (10pct ideal capacity)" $
              -- this is our largest possible cache size will necessarily evict
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions $ Just maxBound) $ \cache ->
                go threadsPerHEC cache payloads
          , bench "bounded 6000 (1pct ideal capacity)" $
              perRunEnv (Cache.initialise $ Cache.mkCacheOptions $ Just 6000) $ \cache ->
                go threadsPerHEC cache payloads
          ]
  ]
  where
    go :: Int -> Cache.Cache Int Int -> [Int] -> IO ()
    go threadFactor cache payload = do
      bef <- getMonotonicTimeNSec
      -- So that `go 0 ...`  will give us a single thread:
      threads <- (+ 1) . (* threadFactor) <$> getNumCapabilities
      -- each thread takes its own interleaved section of payload. Try to do
      -- this work before forking.
      let !localPayloads = force $
             map (\tN -> map head $ chunksOf threads $ drop tN payload) [0..(threads-1)]
      _hitsMisses <- forConcurrently localPayloads $ \payloadL -> do
        foldM lookupInsertLoop (0,0) payloadL
      aft <- getMonotonicTimeNSec
      -- TODO we need to decide whether to rewrite these benchmarks or fix
      -- criterion so it can support what I want here (to run a slow benchmark
      -- perhaps one time, with an actual time limit).
      --   We should also look into just generating a report by hand that takes
      -- into account per-thread misses without actually simulating them with
      -- burnCycles.
      putStrLn $ "TIMING: " <>(show $ fromIntegral (aft - bef) / (1000*1000 :: Double)) <> "ms"
      -- putStrLn $ "HITS/MISSES: "<> show _hitsMisses  -- DEBUGGING/FYI
      return ()
      where
        lookupInsertLoop :: (Int, Int) -> Int -> IO (Int, Int)
        lookupInsertLoop (!h, !m) p = do
          Cache.lookup p cache >>= \case
            -- happy path: item was in the cache:
            Just !_ -> return (h+1, m)
            -- sad path: Do some work to simulate cost of a cache miss before caching:
            Nothing -> do
              -- add some jitter to workload:
              let jRange = wrk `div` 4 -- tunable
                  -- assumes `p` is random:
                  wrkJittered
                    | wrk == 0 = 0
                    | otherwise = wrk + ((p `mod` jRange) - (jRange `div` 2))
              !_ <- burnCycles wrkJittered
              Cache.insert p p cache
              return (h, m+1)



-- | Do some work, that scales linearly proportional to N and hopefully won't
-- be optimized away. We also make sure to allocate to ensure runtime can
-- deschedule threads running this.
--
-- This is tuned to take 100ns on my machine.
--
-- NOTE: it would be nice (maybe) if we could just tell criterion that we want
-- to fake some extra time added to a particular benchmark run.
burnCycles :: Int -> IO Int
{-# NOINLINE burnCycles #-}
burnCycles = go 0XBEEF where
  go !x !n
    | n <= 0 = return x
    | otherwise = do
        uselessRef <- newIORef x
        let pureWork = 73 -- arbitrary, for fine-tuning
            !x' = foldl' (\acc b-> (acc `xor` b) * 1099511628211) x [1..pureWork]
        x'' <- readIORef uselessRef
        go (x' `xor` x'') (n-1)



readBenches :: Int -> Benchmark
readBenches n =
  bgroup ("size "<>show n) [
    env (populate n U.initialise U.insertAllStripes) $ \ ~(cache, k)->
      bgroup "unbounded" [
        bench "hit" $
          nfAppIO (\k' -> U.lookup k' cache) k
      , bench "miss" $
          nfAppIO (\k' -> U.lookup k' cache) 0xDEAD
      ]
  , env (populate n (B.initialise (fromIntegral $ n*2)) B.insertAllStripes) $ \ ~(cache, k)->
      bgroup "bounded" [
        bench "hit" $
          nfAppIO (\k' -> B.lookup k' cache) k
      , bench "miss" $
          nfAppIO (\k' -> B.lookup k' cache) 0xDEAD
      ]
  ]


-- return a randomly-populated cache, along with an item somewhere in the middle.
-- We take care to use random keys since Hashable is untrustworthy.
populate :: Int -> IO cache -> (Int -> Int -> cache -> IO b) -> IO (cache, Int)
populate n _initialise _insertAllStripes = do
  cache <- _initialise
  rs <- randomInts n
  mapM_ (\k -> _insertAllStripes k k cache) rs
  let medianish = V.minimumBy (comparing abs) rs
  return (cache, medianish)


randomInts :: Int -> IO (V.Vector Int)
randomInts n =
  withSystemRandom . asGenST $ \gen -> uniformVector gen n


-- | Return a zipf-mandelbrot distributed list of 'n' Ints (the Ints themselves
-- will be uniformly random, see 'randomInts'). The first parameter controls
-- the skew. The two returned Double values are:
--   - number of unique values in list (i.e. max cache residency)
--   - number of cache hits assuming an unbounded cache, no races or striping
--   - pct of samples falling into most frequent 1% bucket
--   - pct of samples falling into most frequent 10% bucket
--
-- These can be used to try to pick some reasonable skew parameter (I'm not
-- sure how to do that more scientifically).
--
-- This is slow, and as skew gets closer to 1 (e.g. 1.0001) this becomes very
-- slow, which is a shame because these seem most realistic.
zipfianRandomInts :: Int -> Double -> IO ([Int], Int, Int, Double, Double)
zipfianRandomInts n sk = do
  gen <- Rand.createSystemRandom
  payloadVals <- randomInts $ 100*1000
  zipfIxs <- Rand.samples n (Rand.zipf sk) gen :: IO [Word32]
  let groupings = reverse $ sort $ map length $ group $ sort zipfIxs
      uniqs = length groupings
      top buckets =
        let inTop = sum $ take ((uniqs `div` buckets) + 1) groupings
         in fromIntegral inTop / fromIntegral n :: Double
      idealHits = sum $ map (subtract 1) groupings
  payloads <- for zipfIxs $ \ix_w32 ->
    case payloadVals V.!? fromIntegral ix_w32 of
      -- we could generate a random val here, but this seems fine:
      Nothing -> pure $ fromIntegral ix_w32
      Just x  -> pure x
  return (payloads, uniqs, idealHits, top 10, top 100)


-- noops, orphans:
instance NFData (B.BoundedCache k v) where
  rnf _ = ()
instance NFData (U.UnboundedCache k v) where
  rnf _ = ()
instance NFData (Cache.Cache k v) where
  rnf _ = ()
