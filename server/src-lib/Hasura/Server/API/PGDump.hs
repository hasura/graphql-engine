-- | API related to Postgres' pg dump
module Hasura.Server.API.PGDump
  ( PGDumpReqBody(..)
  , execPGDump
  ) where

import           Control.Exception      (IOException, try)
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy    as BL
import           Data.Char               (isSpace)
import qualified Data.List               as L
import qualified Data.Text               as T
import           Data.Text.Conversions
import qualified Database.PG.Query       as Q
import           Hasura.Prelude
import qualified Hasura.RQL.Types.Error as RTE
import           System.Exit
import           System.Process
import qualified Text.Regex.TDFA        as TDFA

data PGDumpReqBody =
  PGDumpReqBody
  { prbOpts        :: ![String]
  , prbCleanOutput :: !(Maybe Bool)
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGDumpReqBody)

execPGDump
  :: (MonadError RTE.QErr m, MonadIO m)
  => PGDumpReqBody
  -> Q.ConnInfo
  -> m BL.ByteString
execPGDump b ci = do
  eOutput <- liftIO $ try execProcess
  output <- either throwException return eOutput
  case output of
    Left err ->
      RTE.throw500 $ "error while executing pg_dump: " <> err
    Right dump -> return dump
  where
    throwException :: (MonadError RTE.QErr m) => IOException -> m a
    throwException _ = RTE.throw500 "internal exception while executing pg_dump"

    execProcess = do
      (exitCode, stdOut, stdErr) <- readProcessWithExitCode "pg_dump" opts ""
      return $ case exitCode of
        ExitSuccess   -> Right $ unUTF8 $ convertText (clean stdOut)
        ExitFailure _ -> Left $ toText stdErr

    connString = T.unpack $ bsToTxt $ Q.pgConnString $ Q.ciDetails ci
    opts = connString : "--encoding=utf8" : prbOpts b

    clean str
      | fromMaybe False (prbCleanOutput b) =
          unlines $ filter (not . shouldDropLine) (lines str)
      | otherwise = str

    shouldDropLine line =
      -- delete empty lines
      all isSpace line
        -- delete comments
        || "--" `L.isPrefixOf` line
        -- delete front matter
        || line `elem` preambleLines
        -- delete notify triggers
        || eventTriggerRegex `TDFA.match` line

    preambleLines =
      [ "SET statement_timeout = 0;"
      , "SET lock_timeout = 0;"
      , "SET idle_in_transaction_session_timeout = 0;"
      , "SET client_encoding = 'UTF8';"
      , "SET standard_conforming_strings = on;"
      , "SELECT pg_catalog.set_config('search_path', '', false);"
      , "SET check_function_bodies = false;"
      , "SET xmloption = content;"
      , "SET client_min_messages = warning;"
      , "SET row_security = off;"
      , "SET default_tablespace = '';"
      , "SET default_with_oids = false;"
      , "SET default_table_access_method = heap;"
      , "CREATE SCHEMA public;"
      , "COMMENT ON SCHEMA public IS 'standard public schema';"
      ]

    eventTriggerRegex =
      let regexStr :: String =
        -- pg functions created by hasura for event triggers used "notify_hasura"
        -- These changes are also documented on the method pgIdenTrigger
            "^CREATE TRIGGER \"?notify_hasura_.+\"? AFTER [[:alnum:]]+ "
              <> "ON .+ FOR EACH ROW EXECUTE (FUNCTION|PROCEDURE) "
              <> "\"?hdb_views\"?\\.\"?notify_hasura_.+\"?\\(\\);$"
      in TDFA.makeRegex regexStr :: TDFA.Regex
