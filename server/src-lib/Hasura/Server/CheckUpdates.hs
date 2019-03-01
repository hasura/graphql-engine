module Hasura.Server.CheckUpdates
  ( checkForUpdates
  ) where

import           Control.Exception     (try)
import           Control.Lens
import           Control.Monad         (forever)
import           System.Environment    (lookupEnv)

import qualified Control.Concurrent    as C
import qualified Data.Aeson            as A
import qualified Data.Aeson.Casing     as A
import qualified Data.Aeson.TH         as A
import qualified Data.Text             as T
import qualified Network.HTTP.Client   as H
import qualified Network.Wreq          as Wreq
import qualified System.Log.FastLogger as FL

import           Hasura.HTTP
import           Hasura.Logging        (LoggerCtx (..))
import           Hasura.Prelude
import           Hasura.Server.Version (currentVersion)


newtype UpdateInfo
  = UpdateInfo
  { _uiLatest :: T.Text
  } deriving (Show, Eq)

$(A.deriveJSON (A.aesonDrop 2 A.snakeCase) ''UpdateInfo)

checkForUpdates :: LoggerCtx -> H.Manager -> IO ()
checkForUpdates (LoggerCtx loggerSet _ _) manager = do
  let options = wreqOptions manager []
  url <- getUrl
  forever $ do
    resp <- try $ Wreq.getWith options $ T.unpack url
    case resp of
      Left ex -> ignoreHttpErr ex
      Right bs -> do
        UpdateInfo latestVersion <- decodeResp $ bs ^. Wreq.responseBody
        when (latestVersion /= currentVersion) $
          FL.pushLogStrLn loggerSet $ FL.toLogStr $ updateMsg latestVersion

    C.threadDelay aDay

  where
    updateMsg v = "Update: A new version is available: " <> v
    getUrl = do
      let buildUrl a = "https://releases.hasura.io/graphql-engine?agent="
                       <> a
                       <> "&version="
                       <> currentVersion
      isCI <- lookupEnv "CI"
      case isCI of
        Just "true" -> return $ buildUrl "server-ci"
        _           -> return $ buildUrl "server"

    aDay = 86400 * 1000 * 1000

    -- ignoring if there is any error in response and returning the current version
    decodeResp bs = case A.eitherDecode bs of
      Left _  -> return $ UpdateInfo currentVersion
      Right a -> return a

    ignoreHttpErr :: H.HttpException -> IO ()
    ignoreHttpErr _ = return ()
