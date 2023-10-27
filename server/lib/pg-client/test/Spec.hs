{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use withAsync" #-}
{-# HLINT ignore "Use sleep" #-}

module Main (main) where

-------------------------------------------------------------------------------

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BS
import Data.String (fromString)
import Database.PG.Query
import Interrupt (specInterrupt)
import Jsonb (specJsonb)
import System.Environment qualified as Env
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)
import Timeout (specTimeout)
import Prelude

-------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore avoid Control.Concurrent.forkIO" :: String) #-}

{-# ANN module ("HLint: ignore avoid Control.Concurrent.threadDelay" :: String) #-}

{- Note [Running tests]
~~~~~~~~~~~~~~~~~~~~~~~
The tests in this module expect a postgres instance running. No setup is
required, these tests do not run any query on the database. The only requirement
is that the environment variable DATABASE_URL is set such that it is a valid
connection string to this instance (e.g.
"postgresql://user:pass@127.0.0.1:5432/instance?sslmode=disable").

TODO: run these tests as part of CI.
-}

main :: IO ()
main = hspec $ do
  describe "acquiring connections" do
    it "acquire a single available resource" do
      simpleTest `shouldReturn` Nothing
    it "error when no connections available" do
      noConnectionAvailable `shouldReturn` Nothing
    it "release and acquire works correctly" do
      releaseAndAcquire `shouldReturn` Nothing
    it "release and acquire works correctly" do
      releaseAndAcquireWithTimeout `shouldReturn` Nothing
    it "time out works correctly" do
      releaseAndAcquireWithTimeoutNegative `shouldReturn` Nothing

    let uriConnDetails :: ConnDetails
        uriConnDetails =
          CDDatabaseURI $
            fromString
              "postgresql://user:pass@127.0.0.1:5432/instance?sslmode=disable"

    it "parses a host name correctly" do
      h <- extractHost uriConnDetails
      h `shouldBe` Just "127.0.0.1"

    it "parses connection options correctly" do
      extractConnOptions uriConnDetails
        `shouldBe` Just
          ConnOptions
            { connHost = "127.0.0.1",
              connPort = 5432,
              connUser = "user",
              connPassword = "pass",
              connDatabase = "instance",
              connOptions = Nothing
            }

  specInterrupt
  specTimeout
  specJsonb

mkPool :: IO PGPool
mkPool = do
  dbUri <- BS.pack <$> Env.getEnv "DATABASE_URL"
  initPGPool (connInfo dbUri) J.Null connParams logger
  where
    connInfo uri =
      ConnInfo
        { ciRetries,
          ciDetails = mkDetails uri
        }
    ciRetries = 0
    mkDetails = CDDatabaseURI
    logger = mempty
    connParams = ConnParams 1 1 60 True Nothing (Just 3) False

withFreshPool ::
  (FromPGConnErr e) =>
  PGPool ->
  IO a ->
  IO (Either e a)
withFreshPool pool action =
  runExceptT
    . withConn pool
    . const
    $ lift action

err :: (Show a) => a -> IO (Maybe String)
err = pure . Just . show

nada :: IO ()
nada = mempty

simpleTest :: IO (Maybe String)
simpleTest = do
  pool <- mkPool
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _ -> mempty

noConnectionAvailable :: IO (Maybe String)
noConnectionAvailable = do
  pool <- mkPool
  withFreshPool pool (action pool) >>= \case
    Left (e :: PGExecErr) -> err e
    Right _ -> mempty
  where
    action pool =
      withFreshPool pool nada >>= \case
        Left (_ :: PGExecErr) -> mempty
        Right _ -> err "connection acquisition expected to fail"

releaseAndAcquire :: IO (Maybe String)
releaseAndAcquire = do
  pool <- mkPool
  _ <-
    withFreshPool pool nada >>= \case
      Left (e :: PGExecErr) -> err e
      Right _ -> mempty
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _ -> mempty

releaseAndAcquireWithTimeout :: IO (Maybe String)
releaseAndAcquireWithTimeout = do
  pool <- mkPool
  _ <-
    forkIO $
      withFreshPool pool (threadDelay 300_000) >>= \case
        Left (_ :: PGExecErr) -> error "unexpected error when acquiring connections"
        Right _ -> mempty
  threadDelay 100_000
  withFreshPool pool nada >>= \case
    Left (e :: PGExecErr) -> err e
    Right _ -> mempty

releaseAndAcquireWithTimeoutNegative :: IO (Maybe String)
releaseAndAcquireWithTimeoutNegative = do
  pool <- mkPool
  _ <-
    forkIO $
      withFreshPool pool (threadDelay 10_000_000) >>= \case
        Left (_ :: PGExecErr) -> error "unexpected error when acquiring connections"
        Right _ -> mempty
  threadDelay 1_000_000
  withFreshPool pool nada >>= \case
    Left (_ :: PGExecErr) -> mempty
    Right _ -> err "Wat"
