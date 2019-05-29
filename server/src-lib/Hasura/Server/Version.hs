module Hasura.Server.Version
  ( currentVersion
  , consoleVersion
  , isDevVersion
  )
where

import           Control.Lens        ((^.), (^?))
import           Data.Either         (isLeft)

import qualified Data.SemVer         as V
import qualified Data.Text           as T

import           Hasura.Prelude
import           Hasura.Server.Utils (getValFromEnvOrScript)

version :: T.Text
version = T.dropWhileEnd (== '\n')
  $(getValFromEnvOrScript "VERSION" "../scripts/get-version.sh")

parsedVersion :: Either String V.Version
parsedVersion = V.fromText $ T.dropWhile (== 'v') version

currentVersion :: T.Text
currentVersion = version

isDevVersion :: Bool
isDevVersion = isLeft parsedVersion

rawVersion :: T.Text
rawVersion = "versioned/" <> version

consoleVersion :: T.Text
consoleVersion = case parsedVersion of
  Left _  -> rawVersion
  Right v -> mkConsoleV v

mkConsoleV :: V.Version -> T.Text
mkConsoleV v = case getReleaseChannel v of
  Nothing -> rawVersion
  Just c  -> T.pack $ "channel/" <> c <> "/" <> vMajMin
  where
    vMajMin = "v" <> show (v ^. V.major) <> "." <> show (v ^. V.minor)

getReleaseChannel :: V.Version -> Maybe String
getReleaseChannel sv = case sv ^. V.release of
  [] -> Just "stable"
  (mr:_) -> case getTextFromId mr of
    Nothing -> Nothing
    Just r -> case T.unpack r of
      ('a':'l':'p':'h':'a':_) -> Just "alpha"
      ('b':'e':'t':'a':_)     -> Just "beta"
      ('r':'c':_)             -> Just "rc"
      _                       -> Nothing

getTextFromId :: V.Identifier -> Maybe T.Text
getTextFromId i = Just i ^? (toTextualM . V._Textual)
  where
    toTextualM _ Nothing  = pure Nothing
    toTextualM f (Just a) = f a
