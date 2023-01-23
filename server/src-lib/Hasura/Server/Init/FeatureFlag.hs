{-# LANGUAGE DeriveAnyClass #-}

-- | Feature Flags are /temporary/ toggles.
module Hasura.Server.Init.FeatureFlag
  ( FeatureFlag (..),
    defaultValueIO,
    Identifier (..),
    FeatureFlags (..),
    featureFlags,
    nativeQueryInterface,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
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
    ffDescription :: Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

-- | We hardcode all feature flags to their default value in OSS.
defaultValueIO :: FeatureFlag -> IO Bool
defaultValueIO = pure . ffDefaultValue

--------------------------------------------------------------------------------

newtype FeatureFlags = FeatureFlags {getFeatureFlags :: HashMap Text FeatureFlag}

featureFlags :: FeatureFlags
featureFlags =
  FeatureFlags $
    HashMap.fromList
      [ ("test-flag", testFlag),
        ("native-query-interface", nativeQueryInterface)
      ]

--------------------------------------------------------------------------------

testFlag :: FeatureFlag
testFlag =
  FeatureFlag
    { ffIdentifier = Identifier "test-flag",
      ffDefaultValue = False,
      ffDescription = "Testing feature flag integration"
    }

nativeQueryInterface :: FeatureFlag
nativeQueryInterface =
  FeatureFlag
    { ffIdentifier = Identifier "native-query-interface",
      ffDefaultValue = False,
      ffDescription = "Expose custom views, permissions and advanced SQL functionality via custom queries"
    }
