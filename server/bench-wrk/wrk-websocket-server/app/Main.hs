{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad                (fail, void)

import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Network.WebSockets           as WS
import qualified System.Directory             as Dir

import           Wrk.Server

main :: IO ()
main = do
  void checkIfWrksPresent
  lock <- TMVar.newEmptyTMVarIO
  print "Running wrk websocket server or port 9160"
  WS.runServer "127.0.0.1" 9160 $ benchWsApp lock

checkIfWrksPresent :: IO [FilePath]
checkIfWrksPresent = mapM findExec ["wrk", "wrk2"]
  where
    findExec e = Dir.findExecutable e >>= maybe (noExecErr e) return
    noExecErr e = fail $ "Could not find executable " <> e

