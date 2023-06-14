{-# LANGUAGE DeriveAnyClass #-}

-- | Feature Flags are /temporary/ toggles.
module Hasura.Server.Init.FeatureFlag
  ( FeatureFlag (..),
    CheckFeatureFlag (..),
    checkFeatureFlag,
    Identifier (..),
    FeatureFlags (..),
    HasFeatureFlagChecker (..),
    featureFlags,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype Identifier = Identifier {getIdentifier :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, FromJSON, ToJSON)
  deriving anyclass (Hashable)

data FeatureFlag = FeatureFlag
  { ffIdentifier :: Identifier,
    ffDefaultValue :: Bool,
    ffDescription :: Text,
    ffEnvVar :: String
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

-- | In OSS we look for a environment variable or fall back to the default
-- value
checkFeatureFlag :: Env.Environment -> FeatureFlag -> IO Bool
checkFeatureFlag env (FeatureFlag {ffEnvVar = envVar, ffDefaultValue = defaultValue}) =
  case Env.lookupEnv env envVar of
    Just found -> pure $ fromMaybe defaultValue (readMaybe found)
    Nothing -> pure $ defaultValue

newtype CheckFeatureFlag = CheckFeatureFlag {runCheckFeatureFlag :: FeatureFlag -> IO Bool}

--------------------------------------------------------------------------------

newtype FeatureFlags = FeatureFlags {getFeatureFlags :: HashMap Text FeatureFlag}

featureFlags :: FeatureFlags
featureFlags =
  FeatureFlags
    $ HashMap.fromList
      [ ("test-flag", testFlag)
      ]

--------------------------------------------------------------------------------

class (Monad m) => HasFeatureFlagChecker m where
  checkFlag :: FeatureFlag -> m Bool

instance (HasFeatureFlagChecker m) => HasFeatureFlagChecker (ReaderT r m) where
  checkFlag = lift . checkFlag

instance (HasFeatureFlagChecker m) => HasFeatureFlagChecker (ExceptT e m) where
  checkFlag = lift . checkFlag

instance (HasFeatureFlagChecker m) => HasFeatureFlagChecker (StateT s m) where
  checkFlag = lift . checkFlag

--------------------------------------------------------------------------------

testFlag :: FeatureFlag
testFlag =
  FeatureFlag
    { ffIdentifier = Identifier "test-flag",
      ffDefaultValue = False,
      ffDescription = "Testing feature flag integration",
      ffEnvVar = "HASURA_FF_TEST_FLAG"
    }
