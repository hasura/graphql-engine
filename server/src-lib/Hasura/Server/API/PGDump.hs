-- | API related to Postgres' pg dump
module Hasura.Server.API.PGDump
  ( PGDumpReqBody (..),
    execPGDump,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Conversions
import Database.PG.Query qualified as PG
import Hasura.Base.Error qualified as RTE
import Hasura.Prelude
import Hasura.RQL.Types.Common
import System.Exit
import System.Process
import Text.Regex.TDFA qualified as TDFA

data PGDumpReqBody = PGDumpReqBody
  { prbSource :: !SourceName,
    prbOpts :: ![String],
    prbCleanOutput :: !Bool
  }
  deriving (Show, Eq)

instance FromJSON PGDumpReqBody where
  parseJSON = withObject "Object" $ \o ->
    PGDumpReqBody
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "opts"
      <*> o
      .:? "clean_output"
      .!= False

execPGDump ::
  (MonadError RTE.QErr m, MonadIO m) =>
  PGDumpReqBody ->
  PG.ConnInfo ->
  m BL.ByteString
execPGDump b ci = do
  eOutput <- liftIO $ try @IOException execProcess
  output <- eOutput `onLeft` (throwException "internal exception while executing pg_dump" . show)
  output `onLeft` throwException "error while executing pg_dump"
  where
    throwException :: (MonadError RTE.QErr m, ToJSON e) => Text -> e -> m a
    throwException text err = throwError (RTE.err500 RTE.Unexpected text) {RTE.qeInternal = Just (RTE.ExtraInternal (toJSON err))}

    execProcess = do
      connString <- T.unpack . bsToTxt <$> (PG.pgConnString $ PG.ciDetails ci)
      let opts = connString : "--encoding=utf8" : prbOpts b
      (exitCode, stdOut, stdErr) <- readProcessWithExitCode "pg_dump" opts ""
      return $ case exitCode of
        ExitSuccess -> Right $ unUTF8 $ convertText (clean stdOut)
        ExitFailure _ -> Left $ toText stdErr

    clean str
      | prbCleanOutput b =
          unlines $ filter (not . shouldDropLine) (lines str)
      | otherwise = str

    shouldDropLine line =
      -- delete empty lines
      all isSpace line
        -- delete comments
        || ("--" `L.isPrefixOf` line)
        -- delete front matter
        || (line `elem` preambleLines)
        -- delete notify triggers
        || (eventTriggerRegex `TDFA.match` line)

    preambleLines =
      [ "SET statement_timeout = 0;",
        "SET lock_timeout = 0;",
        "SET idle_in_transaction_session_timeout = 0;",
        "SET client_encoding = 'UTF8';",
        "SET standard_conforming_strings = on;",
        "SELECT pg_catalog.set_config('search_path', '', false);",
        "SET xmloption = content;",
        "SET client_min_messages = warning;",
        "SET row_security = off;",
        "SET default_tablespace = '';",
        "SET default_with_oids = false;",
        "SET default_table_access_method = heap;",
        "CREATE SCHEMA public;",
        "COMMENT ON SCHEMA public IS 'standard public schema';"
      ]

    eventTriggerRegex =
      let regexStr :: String =
            -- pg functions created by hasura for event triggers used "notify_hasura"
            -- These changes are also documented on the method pgIdenTrigger
            "^CREATE TRIGGER \"?notify_hasura_.+\"? AFTER [[:alnum:]]+ "
              <> "ON .+ FOR EACH ROW EXECUTE (FUNCTION|PROCEDURE) "
              <> "\"?hdb_catalog\"?\\.\"?notify_hasura_.+\"?\\(\\);$"
       in TDFA.makeRegex regexStr :: TDFA.Regex
