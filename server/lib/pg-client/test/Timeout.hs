{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Timeout (specTimeout) where

-------------------------------------------------------------------------------

import Control.Concurrent.Async (async, wait)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Int (Int32)
import Data.Time (diffUTCTime, getCurrentTime)
import Database.PG.Query
import System.Environment qualified as Env
import System.Timeout (timeout)
import Test.Hspec (Spec, before, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Prelude

-------------------------------------------------------------------------------

specTimeout :: Spec
specTimeout = before initDB $ do
  describe "slow insert" $ do
    it "inserts a row successfully if not interrupted" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- sleepyInsert pool 1
      t1 <- getCurrentTime
      res `shouldBe` Right ()
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 1 && x < 2)
      countRows pool `shouldReturn` 1

    it "is interrupted late by timeout" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ sleepyInsert pool 1
      t1 <- getCurrentTime
      -- timed out
      res `shouldBe` Nothing
      -- but still took the full second
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 1 && x < 2)
    -- insert was rolled back
    -- countRows pool `shouldReturn` 0
    -- [note] This is true, but not something we want to assert.
    -- 'runTx' runs independent IO actions to BEGIN and COMMIT
    -- around the query. The async exception is delivered as
    -- soon as the blocking FFI call for the query itself returns,
    -- so neither COMMIT nor ABORT from 'asTransaction' get sent.

    it "is not rolled back with async" $ \pool -> do
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ do
        a <- async $ sleepyInsert pool 1
        wait a
      t1 <- getCurrentTime
      -- timed out
      res `shouldBe` Nothing
      -- quickly
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 0.5 && x < 0.75)
      -- but the insert went through
      countRows pool `shouldReturn` 1

    it "is interrupted promptly with cancelling" $ \pool -> do
      cancelablePool <- mkPool True
      countRows pool `shouldReturn` 0
      t0 <- getCurrentTime
      res <- timeout 500000 $ sleepyInsert cancelablePool 1
      t1 <- getCurrentTime
      res `shouldBe` Nothing
      -- promptly
      diffUTCTime t1 t0 `shouldSatisfy` (\x -> x >= 0.5 && x < 0.75)
      -- insert was rolled back
      -- [note] This relies on the exception being delivered mid-transaction;
      -- it's possible to have 'res == Nothing' (stating the request was timed
      -- out) and have the transaction committed anyway, if the exception is
      -- delivered after sending 'COMMIT' but before returning.
      countRows pool `shouldReturn` 0

mkPool :: Bool -> IO PGPool
mkPool cancelable = do
  dbUri <- BS.pack <$> Env.getEnv "DATABASE_URL"
  initPGPool (connInfo dbUri) J.Null connParams logger
  where
    logger = print
    connInfo uri =
      ConnInfo
        { ciRetries = 0,
          ciDetails = CDDatabaseURI uri
        }
    connParams =
      ConnParams
        { cpStripes = 1,
          cpConns = 1,
          cpIdleTime = 60,
          cpAllowPrepare = True,
          cpMbLifetime = Nothing,
          cpTimeout = Nothing,
          cpCancel = cancelable
        }

mode :: TxMode
mode = (Serializable, Just ReadWrite)

initDB :: IO PGPool
initDB = do
  pool <- mkPool False
  let tx = multiQE PGExecErrTx (fromText statements)
  res <- runExceptT $ runTx pool mode tx
  res `shouldBe` Right ()
  return pool
  where
    statements =
      "DROP TABLE IF EXISTS test_timeout;\n\
      \CREATE TABLE test_timeout (x int);\n\
      \CREATE OR REPLACE FUNCTION sleepy(int) RETURNS int\n\
      \  LANGUAGE sql AS\n\
      \$$\n\
      \  select pg_sleep($1);\n\
      \  select $1\n\
      \$$;\n"

countRows :: PGPool -> IO Int
countRows pool = do
  Right count <- runExceptT $ runIdentity . getRow <$> runTx pool mode tx
  return count
  where
    query = "SELECT count(*) FROM test_timeout"
    tx = withQE PGExecErrTx (fromText query) () False

sleepyInsert :: PGPool -> Int32 -> IO (Either PGExecErr ())
sleepyInsert pool sleep =
  runExceptT $ runTx pool mode tx
  where
    query = "INSERT INTO test_timeout VALUES (sleepy($1))"
    tx = withQE PGExecErrTx (fromText query) (Identity sleep) False
