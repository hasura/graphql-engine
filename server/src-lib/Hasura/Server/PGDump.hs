{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.PGDump
  ( PGDumpReqBody
  , executePGDump
  ) where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.FileEmbed
import           Debug.Trace
import           Hasura.Prelude
import           System.Exit
import           System.Process

data PGDumpReqBody
  = PGDumpReqBody
  { prbSchema :: !String
  , prgOpts   :: !(Maybe String)
  }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGDumpReqBody)

script :: IsString a => a
script = $(embedStringFile "src-rsr/run_pg_dump.sh")

-- executePGDump :: PGDumpReqBody -> IO (Either String (FilePath, FileOffset, String))
executePGDump :: PGDumpReqBody -> IO (Either String (FilePath, String))
executePGDump _ = do
  (exitCode, filename, stdErr) <- readProcessWithExitCode "/bin/sh" [] script
  case exitCode of
    ExitSuccess   -> do
      -- size <- liftIO $ getFileSize filename
      traceM $ "filename: " <> filename
      contents <- readFile filename
      traceM $ "contents: " <> contents
      return $ Right (filename, contents)
    ExitFailure _ ->  return $ Left stdErr

{--
getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
  stat <- getFileStatus path
  return (fileSize stat)
--}
