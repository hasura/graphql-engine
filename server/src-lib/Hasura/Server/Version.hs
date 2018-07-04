{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.Server.Version
  ( getVersion
  , consoleVersion
  )
where

import           Control.Lens        ((^.))
import           Data.Aeson
import           Web.Spock.Core

import qualified Data.SemVer         as V
import qualified Data.Text           as T

import           Hasura.Prelude
import           Hasura.Server.Utils (jsonHeader, runScript)

version :: T.Text
version = T.dropWhileEnd (== '\n') $ $(runScript "../scripts/get-version.sh")

consoleVersion :: T.Text
consoleVersion = case V.fromText version of
  Right ver -> mkVersion ver
  Left _    -> version

mkVersion :: V.Version -> T.Text
mkVersion ver = T.pack $ "v" ++ show major ++ "." ++ show minor
  where
    major = ver ^. V.major
    minor = ver ^. V.minor

getVersion :: (MonadIO m) => ActionT m ()
getVersion = do
  uncurry setHeader jsonHeader
  lazyBytes $ encode $ object [ "version" .= version ]
