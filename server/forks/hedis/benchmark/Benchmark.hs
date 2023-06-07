{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis
import Text.Printf

nRequests, nClients :: Int
nRequests = 100000
nClients  = 50


main :: IO ()
main = do
    ----------------------------------------------------------------------
    -- Preparation
    --
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        _ <- flushall
        mset [ ("k1","v1"), ("k2","v2"), ("k3","v3")
                        , ("k4","v4"), ("k5","v5") ] >>= \case
          Left _ -> error "error"
          _ -> return ()

        return ()

    ----------------------------------------------------------------------
    -- Spawn clients
    --
    start <- newEmptyMVar
    done  <- newEmptyMVar
    replicateM_ nClients $ forkIO $ do
        runRedis conn $ forever $ do
            action <- liftIO $ takeMVar start
            action
            liftIO $ putMVar done ()

    let timeAction name nActions action = do
          startT <- getCurrentTime
          -- each clients runs ACTION nRepetitions times
          let nRepetitions = nRequests `div` nClients `div` nActions
          replicateM_ nClients $ putMVar start (replicateM_ nRepetitions action)
          replicateM_ nClients $ takeMVar done
          stopT <- getCurrentTime
          let deltaT     = realToFrac $ diffUTCTime stopT startT
              -- the real # of reqs send. We might have lost some due to 'div'.
              actualReqs = nRepetitions * nActions * nClients
              rqsPerSec  = fromIntegral actualReqs / deltaT :: Double
          putStrLn $ printf "%-20s %10.2f Req/s" (name :: String) rqsPerSec

    ----------------------------------------------------------------------
    -- Benchmarks
    --
    timeAction "ping" 1 $ do
        ping >>= \case
          Right Pong -> return ()
          _ -> error "error"
        return ()

    timeAction "get" 1 $ do
        get "key" >>= \case
          Right Nothing -> return ()
          _ -> error "error"
        return ()

    timeAction "mget" 1 $ do
        mget ["k1","k2","k3","k4","k5"] >>= \case
          Right vs -> do
            let expected = map Just ["v1","v2","v3","v4","v5"]
            case vs == expected of
              True -> return ()
              _ -> error "error"
            return ()
          _ -> error "error"

    timeAction "ping (pipelined)" 100 $ do
        pongs <- replicateM 100 ping
        let expected = replicate 100 (Right Pong)
        case pongs == expected of
              True -> return ()
              _ -> error "error"
        return ()

    timeAction "multiExec get 1" 1 $ do
        multiExec (get "foo") >>= \case
          TxSuccess _ -> return ()
          _ -> error "error"
        return ()

    timeAction "multiExec get 50" 50 $ do
        res <- multiExec $ do
          rs <- replicateM 50 (get "foo")
          return $ fmap length (sequence rs)
        case res of
          TxSuccess 50 -> return ()
          _ -> error "error"
        return ()

    timeAction "multiExec get 1000" 1000 $ do
        res <- multiExec $ do
                            rs <- replicateM 1000 (get "foo")
                            return $ fmap length (sequence rs)
        case res of
          TxSuccess 1000 -> return ()
          _ -> error "error"
        return ()

