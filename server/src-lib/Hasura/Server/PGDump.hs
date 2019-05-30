module Hasura.Server.PGDump
  ( PGDumpReqBody
  , execPGDump
  ) where

import           Control.Exception      (IOException, try)
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as BL
import qualified Data.FileEmbed         as FE
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Database.PG.Query      as Q
import           Hasura.Prelude
import qualified Hasura.RQL.Types.Error as RTE
import           Hasura.Server.Utils    (bsToTxt)
import           System.Exit
import           System.Process

data PGDumpReqBody =
  PGDumpReqBody
  { prbOpts        :: ![String]
  , prbCleanOutput :: !(Maybe Bool)
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGDumpReqBody)

script :: IsString a => a
script = $(FE.embedStringFile "src-rsr/run_pg_dump.sh")

runScript
  :: String
  -> [String]
  -> String
  -> IO (Either String BL.ByteString)
runScript dbUrl opts clean = do
  (exitCode, filename, stdErr) <- readProcessWithExitCode "/bin/sh"
    ["/dev/stdin", dbUrl, unwords opts, clean] script
  case exitCode of
    ExitSuccess   -> do
      contents <- BL.readFile $ L.dropWhileEnd (== '\n') filename
      return $ Right contents
    ExitFailure _ ->  return $ Left stdErr

execPGDump
  :: (MonadError RTE.QErr m, MonadIO m)
  => PGDumpReqBody
  -> Q.ConnInfo
  -> m BL.ByteString
execPGDump b ci = do
  eOutput <- liftIO $ try $ runScript dbUrl opts clean
  output <- either throwException return eOutput
  case output of
    Left err ->
      RTE.throw500 $ "error while executing pg_dump: " <> T.pack err
    Right dump -> return dump
  where
    throwException :: (MonadError RTE.QErr m) => IOException -> m a
    throwException _ = RTE.throw500 "internal exception while executing pg_dump"

    dbUrl = case ci of
      Q.CIDatabaseURI _ uri -> T.unpack $ bsToTxt uri
      Q.CIOptions co        -> optsToUrl co

    -- FIXME(shahidhk): need to add connection options (Q.connOptions) too?
    optsToUrl co = "postgres://" <> Q.connUser co <> ":" <> Q.connPassword co
                   <> "@" <>  Q.connHost co <> ":" <> show (Q.connPort co)
                   <> "/" <> Q.connDatabase co
    opts = prbOpts b
    clean = case prbCleanOutput b of
      Just v  -> show v
      Nothing -> show False
