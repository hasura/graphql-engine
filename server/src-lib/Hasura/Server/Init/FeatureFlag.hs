{-# LANGUAGE DeriveAnyClass #-}

-- | Feature Flags are /temporary/ toggles.
module Hasura.Server.Init.FeatureFlag
  ( FeatureFlag (..),
    CheckFeatureFlag (..),
    ceCheckFeatureFlag,
    HasFeatureFlagChecker (..),
    -- Feature flags
    namingConventionSep2023,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Char
import Data.Environment qualified as Env
import Data.Text qualified as T
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype FeatureFlag = FeatureFlag
  { ffIdentifier :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, FromJSON, ToJSON)

-- | In OSS we _may_ look for a environment variable or fall back to the default
-- value.
ceCheckFeatureFlag :: Env.Environment -> CheckFeatureFlag
ceCheckFeatureFlag env =
  CheckFeatureFlag
    { runCheckFeatureFlag = \cases
        ff@FeatureFlag {ffIdentifier = name}
          | ff `elem` map fst ceFeatureFlags ->
              let envVar = "HASURA_FF_" ++ T.unpack (T.map (hypenToUnderscore . toUpper) name)
               in return $ fromMaybe False $ Env.lookupEnv env envVar >>= readMaybe
        _ -> return False,
      listKnownFeatureFlags = ceFeatureFlags
    }
  where
    hypenToUnderscore '-' = '_'
    hypenToUnderscore c = c

data CheckFeatureFlag = CheckFeatureFlag
  { -- | Action that samples the value of a feature flag.
    -- Different products will want to do different things. For example, the
    -- Cloud product will want to use LaunchDarkly whereas the OSS and non-cloud
    -- EE products will want to sample environment variables.
    runCheckFeatureFlag :: FeatureFlag -> IO Bool,
    -- | A registry of flags that are 'known' by the system. This is used to
    -- inform of feature flag values via the '/v1alpha/config' endpoint, as well
    -- as sampling feature flag values for use in schema code.
    listKnownFeatureFlags :: [(FeatureFlag, Text)]
  }

instance Semigroup CheckFeatureFlag where
  cff1 <> cff2 =
    CheckFeatureFlag
      { runCheckFeatureFlag = \ff -> (||) <$> runCheckFeatureFlag cff1 ff <*> runCheckFeatureFlag cff2 ff,
        listKnownFeatureFlags = listKnownFeatureFlags cff1 ++ listKnownFeatureFlags cff2
      }

--------------------------------------------------------------------------------

-- | This is the list of feature flags that exist in the CE version
ceFeatureFlags :: [(FeatureFlag, Text)]
ceFeatureFlags =
  [ (testFlag, "Testing feature flag integration"),
    (namingConventionSep2023, "The changes to the naming-convention feature that were added in September 2023")
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

-- | Testing feature flag integration
testFlag :: FeatureFlag
testFlag = FeatureFlag {ffIdentifier = "test-flag"}

-- | Feature flag enabling the changes to the naming-convention feature that were added in September 2023.
namingConventionSep2023 :: FeatureFlag
namingConventionSep2023 = FeatureFlag {ffIdentifier = "naming-convention-sep-2023"}
