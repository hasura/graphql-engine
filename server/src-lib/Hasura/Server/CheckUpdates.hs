{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.Server.CheckUpdates
  ( checkForUpdates
  ) where

import           Control.Exception     (try)
import           Control.Monad         (forever)

import qualified Control.Concurrent    as C
import qualified Control.Lens          as L
import qualified Data.Aeson            as A
import qualified Data.Aeson.Casing     as A
import qualified Data.Aeson.TH         as A
import qualified Data.Text             as T
import qualified Network.HTTP.Client   as H
import qualified Network.Wreq          as Wreq
import qualified System.Log.FastLogger as FL

import           Hasura.Prelude
import           Hasura.Server.Version (currentVersion)


newtype UpdateInfo
  = UpdateInfo
  { uiLatest :: T.Text
  } deriving (Show, Eq)

$(A.deriveJSON (A.aesonDrop 2 A.snakeCase) ''UpdateInfo)

checkForUpdates :: IO ()
checkForUpdates = do
  loggerSet <- FL.newStdoutLoggerSet FL.defaultBufSize
  forever $ do
    resp <- try $ Wreq.get $ T.unpack url
    case resp of
      Left ex -> ignoreHttpErr ex
      Right bs -> do
        UpdateInfo latestVersion <- decodeResp $ bs L.^. Wreq.responseBody
        when (latestVersion /= currentVersion) $
          FL.pushLogStrLn loggerSet $ FL.toLogStr ("Update: A new version is available: " <> latestVersion)

    C.threadDelay aDay

  where
    url = "https://releases.hasura.io/graphql-engine?agent=server&version="
          <> currentVersion
    aDay = 86400 * 1000 * 1000

    -- ignoring if there is any error in response and returning the current version
    decodeResp bs = case A.eitherDecode bs of
      Left _  -> return $ UpdateInfo currentVersion
      Right a -> return a

    ignoreHttpErr :: H.HttpException -> IO ()
    ignoreHttpErr _ = return ()
