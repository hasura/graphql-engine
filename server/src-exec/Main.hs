module Main where

import           Hasura.App
import           Hasura.Prelude
import           Hasura.Server.Init

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  (httpManager, instanceId) <- initialiseCtx
  handleCommand hgeCmd rci httpManager instanceId Nothing Nothing
