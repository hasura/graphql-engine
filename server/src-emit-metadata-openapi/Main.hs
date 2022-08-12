module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Hasura.Server.MetadataOpenAPI (metadataOpenAPI)
import Prelude

main :: IO ()
main = do
  LBS.putStr $ encodePretty metadataOpenAPI
  putStrLn ""
