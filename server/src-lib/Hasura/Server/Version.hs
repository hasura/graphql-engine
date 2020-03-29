{-# LANGUAGE ImplicitParams #-}

module Hasura.Server.Version
  ( Version(..)
  , getVersionFromEnvironment

  , HasVersion
  , currentVersion
  , consoleAssetsVersion
  , withVersion
  )
where

import           Hasura.Prelude

import qualified Data.SemVer                as V
import qualified Data.Text                  as T
import qualified Language.Haskell.TH.Syntax as TH

import           Text.Regex.TDFA           ((=~~))
import           Control.Lens               ((^.), (^?))
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Text.Conversions      (FromText (..), ToText (..))

import           Hasura.RQL.Instances       ()
import           Hasura.Server.Utils        (getValFromEnvOrScript)

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
    Left _        -> VersionDev txt
    Right version -> VersionRelease version

instance ToJSON Version where
  toJSON = toJSON . toText

instance FromJSON Version where
  parseJSON = fmap fromText . parseJSON

getVersionFromEnvironment :: TH.Q (TH.TExp Version)
getVersionFromEnvironment = do
  let txt = getValFromEnvOrScript "VERSION" "../scripts/get-version.sh"
  [|| fromText $ T.dropWhileEnd (== '\n') $ T.pack $$(txt) ||]

-- | Lots of random things need access to the current version. It would be very convenient to define
-- @version :: 'Version'@ in this module and export it, and indeed, that’s what we used to do! But
-- that turns out to cause problems: the version is compiled into the executable via Template
-- Haskell, so the Pro codebase runs into awkward problems. Since the Pro codebase depends on this
-- code as a library, it has to do gymnastics to ensure that this library always gets recompiled in
-- order to use the updated version, and that’s really hacky.
--
-- A better solution is to explicitly plumb the version through to everything that needs it, but
-- that would be noisy, so as a compromise we use an implicit parameter. Since implicit parameters
-- are a little cumbersome, we hide the parameter itself behind this 'HasVersion' constraint,
-- 'currentVersion' can be used to access it, and 'withVersion' can be used to bring a version into
-- scope.
type HasVersion = ?version :: Version

currentVersion :: HasVersion => Version
currentVersion = ?version

withVersion :: Version -> (HasVersion => r) -> r
withVersion version x = let ?version = version in x

-- | A version-based string used to form the CDN URL for fetching console assets.
consoleAssetsVersion :: HasVersion => Text
consoleAssetsVersion = case currentVersion of
  VersionDev txt -> "versioned/" <> txt
  VersionRelease v -> case getReleaseChannel v of
    Nothing -> "versioned/" <> vMajMin
    Just c  -> "channel/" <> c <> "/" <> vMajMin
    where
      vMajMin = T.pack ("v" <> show (v ^. V.major) <> "." <> show (v ^. V.minor))
  where
    getReleaseChannel :: V.Version -> Maybe Text
    getReleaseChannel sv = case sv ^. V.release of
      []     -> Just "stable"
      (mr:_) -> case getTextFromId mr of
        Nothing -> Nothing
        Just r  -> if
          | T.null r   -> Nothing
          | otherwise  -> T.pack <$> (getChannelFromPreRelease $ T.unpack r)

    getChannelFromPreRelease :: String -> Maybe String
    getChannelFromPreRelease sv = sv =~~ ("^([a-z]+)"::String)


    getTextFromId :: V.Identifier -> Maybe Text
    getTextFromId i = Just i ^? (toTextualM . V._Textual)
      where
        toTextualM _ Nothing  = pure Nothing
        toTextualM f (Just a) = f a
