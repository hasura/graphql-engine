{-# LANGUAGE TemplateHaskell #-}

module Hasura.Server.Version
  ( Version (..),
    currentVersion,
    consoleAssetsVersion,
  )
where

import Control.Exception
import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.FileEmbed (makeRelativeToProject)
import Data.SemVer qualified as V
import Data.Text qualified as T
import Data.Text.Conversions (FromText (..), ToText (..))
import Hasura.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
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
        -- NOTE: This must work correctly in the presence of a caching! See
        -- graphql-engine.cabal (search for “CURRENT_VERSION”) for details
        -- about our approach here. We could use embedFile but want a nice
        -- error message
        $( do
             versionFileName <- makeRelativeToProject "CURRENT_VERSION"
             addDependentFile versionFileName
             let noFileErr =
                   "\n==========================================================================="
                     <> "\n>>> DEAR HASURIAN: The way we bake versions into the server has "
                     <> "\n>>> changed; You'll need to run the following once in your repo to proceed: "
                     <> "\n>>>  $ echo 12345 > \"$(git rev-parse --show-toplevel)/server/CURRENT_VERSION\""
                     <> "\n===========================================================================\n"
             runIO (readFile versionFileName `onException` error noFileErr) >>= stringE
         )

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
