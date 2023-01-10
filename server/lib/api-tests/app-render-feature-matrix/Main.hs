module Main (main) where

import Data.ByteString qualified as ByteString
import Hasura.FeatureMatrix
import Prelude

main :: IO ()
main = ByteString.interact render
