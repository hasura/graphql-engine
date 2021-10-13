module Hasura.Server.Version
  ( Version (..),
    currentVersion,
    consoleAssetsVersion,
  )
where

import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.SemVer qualified as V
import Data.Text qualified as T
import Data.Text.Conversions (FromText (..), ToText (..))
import Hasura.Prelude
import Hasura.Server.Utils (getValFromEnvOrScript)
import Text.Regex.TDFA ((=~~))

data Version
  = VersionDev !Text
  | VersionRelease !V.Version
  deriving (Show, Eq)

instance ToText Version where
  toText = \case
    VersionDev txt -> txt
    VersionRelease version -> "v" <> V.toText version

instance FromText Version where
  fromText txt = case V.fromText $ T.dropWhile (== 'v') txt of
    Left _ -> VersionDev txt
    Right version -> VersionRelease version

instance ToJSON Version where
  toJSON = toJSON . toText

instance FromJSON Version where
  parseJSON = fmap fromText . parseJSON

currentVersion :: Version
currentVersion =
  fromText $
    T.dropWhileEnd (== '\n') $
      T.pack $
        $$(getValFromEnvOrScript "VERSION" "../scripts/get-version.sh")

-- | A version-based string used to form the CDN URL for fetching console assets.
consoleAssetsVersion :: Text
consoleAssetsVersion = case currentVersion of
  VersionDev txt -> "versioned/" <> txt
  VersionRelease v -> case getReleaseChannel v of
    Nothing -> "versioned/" <> vMajMin
    Just c -> "channel/" <> c <> "/" <> vMajMin
    where
      vMajMin = T.pack ("v" <> show (v ^. V.major) <> "." <> show (v ^. V.minor))
  where
    getReleaseChannel :: V.Version -> Maybe Text
    getReleaseChannel sv = case sv ^. V.release of
      [] -> Just "stable"
      (mr : _) -> case getTextFromId mr of
        Nothing -> Nothing
        Just r ->
          if
              | T.null r -> Nothing
              | otherwise -> T.pack <$> getChannelFromPreRelease (T.unpack r)

    getChannelFromPreRelease :: String -> Maybe String
    getChannelFromPreRelease sv = sv =~~ ("^([a-z]+)" :: String)

    getTextFromId :: V.Identifier -> Maybe Text
    getTextFromId i = Just i ^? (toTextualM . V._Textual)
      where
        toTextualM _ Nothing = pure Nothing
        toTextualM f (Just a) = f a
