{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.PGDump
  ( PGDumpReqBody
  , executePGDump
  ) where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.FileEmbed
import           Hasura.Prelude
import           System.Exit
import qualified Data.ByteString.Lazy                   as BL
import           System.Process
import Data.List

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
executePGDump :: PGDumpReqBody -> IO (Either String (FilePath, BL.ByteString))
executePGDump _ = do
  (exitCode, filename, stdErr) <- readProcessWithExitCode "/bin/sh" [] script
  case exitCode of
    ExitSuccess   -> do
      -- size <- liftIO $ getFileSize filename
      contents <- BL.readFile $ dropWhileEnd (== '\n') filename
      return $ Right (filename, contents)
    ExitFailure _ ->  return $ Left stdErr

{--
getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
  stat <- getFileStatus path
  return (fileSize stat)
--}
