{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Hasura.Server.Version
  ( currentVersion
  , consoleVersion
  , isDevVersion
  )
where

import           Control.Lens        ((^.))

import qualified Data.Text           as T
import qualified Salve               as V

import           Hasura.Prelude
import           Hasura.Server.Utils (getValFromEnvOrScript)

version :: T.Text
version = T.dropWhileEnd (== '\n') $(getValFromEnvOrScript "VERSION1" "../scripts/get-version.sh")

parsedVersion :: Maybe V.Version
parsedVersion = V.parseVersion $ T.unpack $ T.dropWhile (== 'v') version

currentVersion :: T.Text
currentVersion = version

isDevVersion :: Bool
isDevVersion = case parsedVersion of
  Just _  -> False
  Nothing -> True

consoleVersion :: T.Text
consoleVersion = case parsedVersion of
  Nothing -> version
  Just v  -> mkConsoleV v

mkConsoleV :: V.Version -> T.Text
mkConsoleV v = T.pack $ release <> "/v" <> show major <> "." <> show minor
  where
    major = v ^. V.majorLens
    minor = v ^. V.minorLens
    release = case v ^. V.preReleasesLens of
      [] -> "stable"
      (r:_) -> case V.renderPreRelease r of
        ('a':'l':'p':'h':'a':_) -> "alpha"
        ('b':'e':'t':'a':_) -> "beta"
        ('r':'c':_) -> "rc"
        _ -> "unknown"
