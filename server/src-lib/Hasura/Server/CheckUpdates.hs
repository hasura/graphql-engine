module Hasura.Server.CheckUpdates
  ( checkForUpdates
  ) where

import           Control.Exception           (try)
import           Control.Lens
import           Control.Monad               (forever)
import           Data.Text.Conversions       (toText)

import qualified CI
import qualified Control.Concurrent.Extended as C
import qualified Data.Aeson                  as A
import qualified Data.Aeson.Casing           as A
import qualified Data.Aeson.TH               as A
import qualified Data.Text                   as T
import qualified Network.HTTP.Client         as H
import qualified Network.URI.Encode          as URI
import qualified Network.Wreq                as Wreq
import qualified System.Log.FastLogger       as FL

import           Hasura.HTTP
import           Hasura.Logging              (LoggerCtx (..))
import           Hasura.Prelude
import           Hasura.Server.Version       (HasVersion, Version, currentVersion)


newtype UpdateInfo
  = UpdateInfo
  { _uiLatest :: Version
  } deriving (Show)

$(A.deriveJSON (A.aesonDrop 2 A.snakeCase) ''UpdateInfo)

checkForUpdates :: (HasVersion) => LoggerCtx a -> H.Manager -> IO void
checkForUpdates (LoggerCtx loggerSet _ _ _) manager = do
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

    C.sleep $ days 1

  where
    updateMsg v = "Update: A new version is available: " <> toText v
    getUrl = do
      let buildUrl agent = "https://releases.hasura.io/graphql-engine?agent=" <>
                           agent <> "&version=" <> URI.encodeText (toText currentVersion)
      ciM <- CI.getCI
      return . buildUrl $ case ciM of
        Nothing -> "server"
        Just ci -> "server-" <> (T.toLower . T.pack $ show ci)

    -- ignoring if there is any error in response and returning the current version
    decodeResp bs = case A.eitherDecode bs of
      Left _  -> return $ UpdateInfo currentVersion
      Right a -> return a

    ignoreHttpErr :: H.HttpException -> IO ()
    ignoreHttpErr _ = return ()
