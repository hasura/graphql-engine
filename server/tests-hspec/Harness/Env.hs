{-# OPTIONS -Wno-redundant-constraints #-}

-- | Read environment variables
module Harness.Env (getEnvRead, getEnvJSON, getEnvString) where

import Data.Aeson qualified as Aeson
import Data.String
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.Stack
import Hasura.Prelude
import System.Environment (lookupEnv)

-- * API

getEnvRead :: (Read a, Typeable a, HasCallStack) => String -> IO a
getEnvRead var =
  withFrozenCallStack $ do
    readVarValue var =<< getEnv var

getEnvString :: (IsString a, HasCallStack) => String -> IO a
getEnvString var =
  withFrozenCallStack $
    fromString <$> getEnv var

getEnvJSON :: forall a. (Typeable a, Aeson.FromJSON a, HasCallStack) => String -> IO a
getEnvJSON var =
  withFrozenCallStack $ do
    accountString <- getEnv var
    onLeft
      (Aeson.eitherDecode' (fromString accountString))
      ( \err ->
          let expectedType = show (typeRep (Proxy :: Proxy a))
           in error (unlines ["Failure parsing '" <> var <> "' to type '" <> expectedType <> "':", show err])
      )

-- * Helpers

getEnv :: HasCallStack => String -> IO String
getEnv var = do
  value <- lookupEnv var
  onNothing value (error $ "getEnv: " <> var <> " does not exist (no environment variable)")

-- | Read a variable to a specific type.
readVarValue :: forall a. (Read a, Typeable a, HasCallStack) => String -> String -> IO a
readVarValue var value =
  onNothing
    (readMaybe value)
    let expectedType = show (typeRep (Proxy :: Proxy a))
     in error
          ( unwords
              [ "Failure parsing '" <> var <> "'",
                "to type '" <> expectedType <> "';",
                "containing value '" <> show value <> "'."
              ]
          )
