module Hasura.Server.Version
  ( currentVersion
  , consoleVersion
  )
where

import           Control.Lens        ((^.))

import qualified Data.SemVer         as V
import qualified Data.Text           as T

import           Hasura.Prelude
import           Hasura.Server.Utils (runScript)

version :: T.Text
version = T.dropWhileEnd (== '\n') $(runScript "../scripts/get-version.sh")

consoleVersion :: T.Text
consoleVersion = case V.fromText $ T.dropWhile (== 'v') version of
  Right ver -> mkVersion ver
  Left _    -> version

mkVersion :: V.Version -> T.Text
mkVersion ver = T.pack $ "v" ++ show major ++ "." ++ show minor
  where
    major = ver ^. V.major
    minor = ver ^. V.minor

currentVersion :: T.Text
currentVersion = version
