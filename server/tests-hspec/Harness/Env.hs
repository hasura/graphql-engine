{-# OPTIONS -Wno-redundant-constraints #-}

module Harness.Env (getEnvRead, getEnvJSON, getEnvString) where

import Data.Aeson qualified as Aeson
import Data.String
import GHC.Stack
import Hasura.Prelude
import System.Environment (getEnv)

getEnvRead :: (Read a, HasCallStack) => String -> IO a
getEnvRead var = do
  str <- getEnv var
  onNothing
    (readMaybe str)
    ( error
        ( unlines
            ["Failure parsing " <> var, " containing value " <> show str]
        )
    )

getEnvString :: (IsString a, HasCallStack) => String -> IO a
getEnvString var = fromString <$> getEnv var

getEnvJSON :: (Aeson.FromJSON a, HasCallStack) => String -> IO a
getEnvJSON var = do
  accountString <- getEnv var
  onLeft
    (Aeson.eitherDecode' (fromString accountString))
    ( \err ->
        error (unlines ["Failure parsing " <> var <> ":", show err])
    )
