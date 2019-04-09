{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.PGDump
  ( PGDumpReqBody
  , execPGDump
  ) where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as BL
import           Data.FileEmbed
import           Data.List
import qualified Data.Text              as T
import qualified Database.PG.Query      as Q
import           Hasura.Prelude
import qualified Hasura.RQL.Types.Error as RTE
import           System.Exit
import           System.Process

data PGDumpReqBody
  = PGDumpReqBody
  { prbSchema :: !String
  , prbOpts   :: !(Maybe String)
  }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGDumpReqBody)

script :: IsString a => a
script = $(embedStringFile "src-rsr/run_pg_dump.sh")

runScript
  :: String
  -> String
  -> IO (Either String BL.ByteString)
runScript dbUrl schema = do
  (exitCode, filename, stdErr) <- readProcessWithExitCode "/bin/sh"
    ["/dev/stdin", dbUrl, schema] script
  case exitCode of
    ExitSuccess   -> do
      contents <- BL.readFile $ dropWhileEnd (== '\n') filename
      return $ Right contents
    ExitFailure _ ->  return $ Left stdErr

execPGDump
  :: (MonadError RTE.QErr m, MonadIO m)
  => PGDumpReqBody
  -> Q.ConnInfo
  -> m BL.ByteString
execPGDump b ci = do
  output <- liftIO $ runScript dbUrl schema
  case output of
    Left err ->
      RTE.throw500 $ "error while executing pg_dump: " <> T.pack err
    Right dump -> return dump
  where
    -- FIXME(shahidhk): need to add connection options (Q.connOptions) too?
    dbUrl = "postgres://" <> Q.connUser ci <> ":" <> Q.connPassword ci
            <> "@" <>  Q.connHost ci <> ":" <> show (Q.connPort ci)
            <> "/" <> Q.connDatabase ci
    schema = prbSchema b
