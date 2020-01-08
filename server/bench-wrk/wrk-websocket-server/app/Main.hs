{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad                (fail, void)

import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Network.WebSockets           as WS
import qualified Options.Applicative          as Opts
import qualified System.Directory             as Dir

import           Wrk.Server
import           Wrk.Server.Types

main :: IO ()
main = do
  void checkIfWrksPresent
  ServerConf graphqlUrl <- parseArgs
  -- TODO test graphQLUrl
  lock <- TMVar.newEmptyTMVarIO
  _ <- testGraphqlUrl graphqlUrl
  print "Running wrk websocket server or port 9160"
  WS.runServer "127.0.0.1" 9160 $ benchWsApp graphqlUrl lock
  where
    testGraphqlUrl url = runQuery url simpleIntrospectQuery

parseArgs :: IO ServerConf
parseArgs = Opts.execParser opts
  where
    opts = Opts.info (Opts.helper <*> serverOpts)
           ( Opts.fullDesc <>
             Opts.header "Wrk Websocket Server: Execute wrk benchmarks and communicate results over websocket"
           )
    serverOpts = ServerConf <$> graphqlUrl
    graphqlUrl = Opts.strArgument (Opts.metavar "GRAPHQL_URL")


checkIfWrksPresent :: IO [FilePath]
checkIfWrksPresent = mapM findExec ["wrk", "wrk2"]
  where
    findExec e = Dir.findExecutable e >>= maybe (noExecErr e) return
    noExecErr e = fail $ "Could not find executable " <> e

