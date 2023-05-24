-- | Read environment variables
module Harness.Env
  ( getEnvRead,
    getEnvJson,
    getEnvJsonFile,
    getEnvString,
  )
where

import Data.Aeson qualified as J
import Data.String
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.Stack
import Hasura.Prelude
import System.Environment (lookupEnv)

-- * API

-- | Get an environment variable and parse it to a value using 'read'.
getEnvRead :: (Read a, Typeable a, HasCallStack) => String -> IO a
getEnvRead var =
  withFrozenCallStack
    $ getEnvWith var readVarValue

-- | Get an environment variable without parsing it.
getEnvString :: (IsString a, HasCallStack) => String -> IO a
getEnvString var =
  withFrozenCallStack
    $ getEnvWith var (\_ value -> pure (fromString value))

-- | Get a json environment variable and parse it.
getEnvJson :: forall a. (Typeable a, J.FromJSON a, HasCallStack) => String -> IO a
getEnvJson var =
  withFrozenCallStack
    $ getEnvWith var decodeJson

-- | Get a environment variable holding a path to a json file and parse the contents of the file.
getEnvJsonFile :: forall a. (Typeable a, J.FromJSON a, HasCallStack) => String -> IO a
getEnvJsonFile var =
  withFrozenCallStack
    $ getEnvWith var (\var' value -> decodeJson var' =<< readFile value)

-------------------------------------------------------------------------------------------

-- * Helpers

-- | Fetches a a value from an environment variable and applies a function to the variable and value.
getEnvWith :: (HasCallStack) => String -> (String -> String -> IO a) -> IO a
getEnvWith var f =
  withFrozenCallStack $ do
    f var =<< getEnv var

-- | Like 'System.Environment.getEnv', but with 'HasCallStack'.
getEnv :: (HasCallStack) => String -> IO String
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

-- | Takes an environment variable and its corresponding value and tries to decode the value as json.
--
--   May throw an exception if decoding fails.
decodeJson :: forall a. (Typeable a, J.FromJSON a, HasCallStack) => String -> String -> IO a
decodeJson var value =
  onLeft
    (J.eitherDecode' (fromString value))
    ( \err ->
        let expectedType = show (typeRep (Proxy :: Proxy a))
         in error (unlines ["Failure parsing '" <> var <> "' to type '" <> expectedType <> "':", show err])
    )
