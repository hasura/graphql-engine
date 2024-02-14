module Hasura.Server.CheckUpdates
  ( checkForUpdates,
  )
where

import CI qualified
import Control.Concurrent.Extended qualified as C
import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.Conversions (toText)
import Hasura.HTTP
import Hasura.Logging (LoggerCtx, getLoggerSet)
import Hasura.Prelude
import Hasura.Server.Version (Version, currentVersion)
import Network.HTTP.Client qualified as HTTP
import Network.URI.Encode qualified as URI
import Network.Wreq qualified as Wreq
import System.Log.FastLogger qualified as FL

newtype UpdateInfo = UpdateInfo
  { _uiLatest :: Version
  }
  deriving (Show, Generic)

-- note that this is erroneous and should drop three characters or use
-- aesonPrefix, but needs to remain like this for backwards compatibility
instance J.FromJSON UpdateInfo where
  parseJSON = J.genericParseJSON (J.aesonDrop 2 J.snakeCase)

instance J.ToJSON UpdateInfo where
  toJSON = J.genericToJSON (J.aesonDrop 2 J.snakeCase)
  toEncoding = J.genericToEncoding (J.aesonDrop 2 J.snakeCase)

checkForUpdates :: LoggerCtx a -> HTTP.Manager -> IO void
checkForUpdates ctx manager = do
  let options = wreqOptions manager []
  url <- getUrl
  forever $ do
    resp <- try $ Wreq.getWith options $ T.unpack url
    case resp of
      Left ex -> ignoreHttpErr ex
      Right bs -> do
        UpdateInfo latestVersion <- decodeResp $ bs ^. Wreq.responseBody
        when (latestVersion /= currentVersion)
          $ FL.pushLogStrLn (getLoggerSet ctx)
          $ FL.toLogStr
          $ updateMsg latestVersion

    C.sleep $ days 1
  where
    updateMsg v = "Update: A new version is available: " <> toText v
    getUrl = do
      let buildUrl agent =
            "https://releases.hasura.io/graphql-engine?agent="
              <> agent
              <> "&version="
              <> URI.encodeText (toText currentVersion)
      ciM <- CI.getCI
      return . buildUrl $ case ciM of
        Nothing -> "server"
        Just ci -> "server-" <> T.toLower (tshow ci)

    -- ignoring if there is any error in response and returning the current version
    decodeResp = pure . fromRight (UpdateInfo currentVersion) . J.eitherDecode

    ignoreHttpErr :: HTTP.HttpException -> IO ()
    ignoreHttpErr _ = return ()
