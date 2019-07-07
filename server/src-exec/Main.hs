module Main where

import           App
import           Hasura.Prelude
import           Hasura.Server.Init

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  (httpManager, loggerCtx, instanceId, logger, pgLogger) <- initialiseCtx
  handleCommand hgeCmd rci httpManager loggerCtx instanceId logger pgLogger Nothing
