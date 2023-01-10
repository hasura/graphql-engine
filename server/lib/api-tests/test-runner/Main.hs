{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

--------------------------------------------------------------------------------

import Spec qualified as Test
import Test.Hspec qualified as Hspec

--------------------------------------------------------------------------------

-- | Entrypoint for api-tests test-suite.
main :: IO ()
main = Hspec.hspec Test.spec
