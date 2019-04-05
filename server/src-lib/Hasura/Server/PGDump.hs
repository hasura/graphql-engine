{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.PGDump where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import Data.FileEmbed
import           Hasura.Prelude
import System.Process
import System.Exit

data PGDumpReqBody
  = PGDumpReqBody
  { prbSchema :: !String
  , prgOpts   :: !(Maybe String)
  }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGDumpReqBody)

script :: IsString a => a
script = $(embedStringFile "src-rsr/run_pg_dump.sh")

executePGDump :: PGDumpReqBody -> IO (Either String FilePath)
executePGDump _ = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode "/bin/sh" [] script
  case exitCode of
    ExitSuccess -> return $ Right stdOut
    ExitFailure _ ->  return $ Left stdErr
