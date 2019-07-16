module Main where

import           Hasura.App
import           Hasura.Prelude
import           Hasura.Server.Init

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  (httpManager, instanceId) <- initialiseCtx
  -- middlewares and logCallback are Nothing for OSS HGE operations
  handleCommand hgeCmd rci httpManager instanceId Nothing Nothing Nothing
