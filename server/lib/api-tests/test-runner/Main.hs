{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad
import Spec qualified as Test
import SpecHook qualified as Test
import System.Exit
import Test.Hspec.Core.Runner qualified as Hspec

--------------------------------------------------------------------------------

-- | Entrypoint for api-tests test-suite.
main :: IO ()
main = do
  testingMode <- Test.setupTestingMode
  (logger, cleanupLogger) <- Test.setupLogger
  Test.setupGlobalConfig testingMode (logger, cleanupLogger)

  res <- Hspec.hspecWithResult Hspec.defaultConfig Test.spec
  cleanupLogger

  unless (Hspec.summaryFailures res == 0) $ exitFailure
