module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Hasura.Server.MetadataOpenAPI (metadataOpenAPI)
import Prelude

main :: IO ()
main = LBS.putStrLn $ encodePretty metadataOpenAPI
