{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CI.Types
  ( CI (..),
    Vendor (..),
    VendorEnv (..),
    EnvVarName (..),
    EnvVarValue (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH as J
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import Instances.TH.Lift ()
import qualified Language.Haskell.TH.Syntax as TH

data CI
  = CI_APPVEYOR -- http://www.appveyor.com/
  | CI_AZURE_PIPELINES -- https://azure.microsoft.com/en-us/services/devops/pipelines/
  | CI_BAMBOO -- https://www.atlassian.com/software/bamboo/
  | CI_BITBUCKET -- https://bitbucket.org/product/features/pipelines/
  | CI_BITRISE -- https://www.bitrise.io/
  | CI_BUDDY -- https://buddy.works/
  | CI_BUILDKITE -- https://buildkite.com/
  | CI_CIRCLE -- http://circleci.com/
  | CI_CIRRUS -- https://cirrus-ci.org/
  | CI_CODEBUILD -- https://aws.amazon.com/codebuild/
  | CI_CODESHIP -- https://codeship.com/
  | CI_DRONE -- https://drone.io/
  | CI_DSARI -- https://github.com/rfinnie/dsari/
  | CI_GITLAB -- https://about.gitlab.com/gitlab-ci/
  | CI_GOCD -- https://www.go.cd/
  | CI_HUDSON -- http://hudson-ci.org/
  | CI_JENKINS -- https://jenkins-ci.org/
  | CI_MAGNUM -- https://magnum-ci.com/
  | CI_NETLIFY -- https://www.netlify.com/
  | CI_NEVERCODE -- http://nevercode.io/
  | CI_SAIL -- https://sail.ci/
  | CI_SEMAPHORE -- https://semaphoreci.com/
  | CI_SHIPPABLE -- https://www.shippable.com/
  | CI_SOLANO -- https://www.solanolabs.com/
  | CI_STRIDER -- https://strider-cd.github.io/
  | CI_TASKCLUSTER -- http://docs.taskcluster.net/
  | CI_TEAMCITY -- https://www.jetbrains.com/teamcity/
  | CI_TRAVIS -- http://travis-ci.org/
  | CI_UNKNOWN_VENDOR
  deriving (Eq, Show, TH.Lift)

$( J.deriveJSON
     J.defaultOptions {J.constructorTagModifier = drop $ T.length "CI_"}
     ''CI
 )

newtype EnvVarName = EnvVarName {unEnvVarName :: Text}
  deriving
    ( Eq,
      Hashable,
      Show,
      J.FromJSON,
      J.FromJSONKey,
      J.ToJSON,
      J.ToJSONKey,
      TH.Lift
    )

newtype EnvVarValue = EnvVarValue {unEnvVarValue :: Text}
  deriving (Eq, Show, J.FromJSON, J.ToJSON, TH.Lift)

data VendorEnv
  = VendorEnvString !EnvVarName
  | VendorEnvList ![EnvVarName]
  | VendorEnvObject !(HashMap EnvVarName EnvVarValue)
  deriving (Eq, Show)

instance TH.Lift VendorEnv where
  liftTyped (VendorEnvString n) = [||VendorEnvString $$(TH.liftTyped n)||]
  liftTyped (VendorEnvList ns) = [||VendorEnvList $$(TH.liftTyped ns)||]
  liftTyped (VendorEnvObject m) =
    [||VendorEnvObject $ HashMap.fromList $$(TH.liftTyped $ HashMap.toList m)||]

instance J.FromJSON VendorEnv where
  parseJSON val = case val of
    J.String _ -> VendorEnvString <$> J.parseJSON val
    J.Array _ -> VendorEnvList <$> J.parseJSON val
    J.Object _ -> VendorEnvObject <$> J.parseJSON val
    _ ->
      fail
        "expected String, List[String], or Map[String, String] in vendor env"

instance J.ToJSON VendorEnv where
  toJSON val = case val of
    VendorEnvString name -> J.toJSON name
    VendorEnvList list -> J.toJSON list
    VendorEnvObject object -> J.toJSON object

newtype VendorName = VendorName {unVendorName :: Text}
  deriving (Eq, Show, J.FromJSON, J.ToJSON, TH.Lift)

data Vendor = Vendor
  { vendorName :: !VendorName,
    vendorConstant :: !CI,
    vendorEnv :: !VendorEnv
  }
  deriving (Eq, Show, TH.Lift)

$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''Vendor)
