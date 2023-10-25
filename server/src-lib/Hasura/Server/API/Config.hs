{-# LANGUAGE DeriveAnyClass #-}

-- | API related to server configuration
module Hasura.Server.API.Config
-- required by pro
  ( ServerConfig (..),
    runGetConfig,
  )
where

import Data.Aeson qualified as J
import Data.HashSet qualified as Set
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.Prelude
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Auth
import Hasura.Server.Auth.JWT
import Hasura.Server.Init.Config (API (METRICS), AllowListStatus)
import Hasura.Server.Init.FeatureFlag (FeatureFlag (..))
import Hasura.Server.Types (ApolloFederationStatus, ExperimentalFeature)
import Hasura.Server.Version (Version, currentVersion)

data FeatureFlagInfo = FeatureFlagInfo
  { ffiName :: Text,
    ffiDescription :: Text,
    ffiEnabled :: Bool
  }
  deriving (Show, Eq, Generic, Hashable)

instance J.ToJSON FeatureFlagInfo where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data JWTInfo = JWTInfo
  { jwtiClaimsNamespace :: !JWTNamespace,
    jwtiClaimsFormat :: !JWTClaimsFormat,
    jwtiClaimsMap :: !(Maybe JWTCustomClaimsMap)
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON JWTInfo where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data ServerConfig = ServerConfig
  { scfgVersion :: !Version,
    scfgIsFunctionPermissionsInferred :: !Options.InferFunctionPermissions,
    scfgIsRemoteSchemaPermissionsEnabled :: !Options.RemoteSchemaPermissions,
    scfgIsAdminSecretSet :: !Bool,
    scfgIsAuthHookSet :: !Bool,
    scfgIsJwtSet :: !Bool,
    scfgJwt :: ![JWTInfo],
    scfgIsAllowListEnabled :: !AllowListStatus,
    scfgLiveQueries :: !ES.LiveQueriesOptions,
    scfgStreamingQueries :: !ES.SubscriptionsOptions,
    scfgConsoleAssetsDir :: !(Maybe Text),
    scfgExperimentalFeatures :: !(Set.HashSet ExperimentalFeature),
    scfgIsPrometheusMetricsEnabled :: !Bool,
    scfgDefaultNamingConvention :: !NamingCase,
    scfgFeatureFlags :: !(Set.HashSet FeatureFlagInfo),
    scfgIsApolloFederationEnabled :: !ApolloFederationStatus
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON ServerConfig where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

runGetConfig ::
  Options.InferFunctionPermissions ->
  Options.RemoteSchemaPermissions ->
  AuthMode ->
  AllowListStatus ->
  ES.LiveQueriesOptions ->
  ES.SubscriptionsOptions ->
  Maybe Text ->
  Set.HashSet ExperimentalFeature ->
  Set.HashSet API ->
  NamingCase ->
  [(FeatureFlag, Text, Bool)] ->
  ApolloFederationStatus ->
  ServerConfig
runGetConfig
  functionPermsCtx
  remoteSchemaPermsCtx
  am
  allowListStatus
  liveQueryOpts
  streamQueryOpts
  consoleAssetsDir
  experimentalFeatures
  enabledAPIs
  defaultNamingConvention
  featureFlags
  apolloFederationStatus =
    ServerConfig
      currentVersion
      functionPermsCtx
      remoteSchemaPermsCtx
      (isAdminSecretSet am)
      (isAuthHookSet am)
      (isJWTSet am)
      (getJWTInfo am)
      allowListStatus
      liveQueryOpts
      streamQueryOpts
      consoleAssetsDir
      experimentalFeatures
      isPrometheusMetricsEnabled
      defaultNamingConvention
      featureFlagSettings
      apolloFederationStatus
    where
      isPrometheusMetricsEnabled = METRICS `Set.member` enabledAPIs
      featureFlagSettings =
        Set.fromList
          $ ( \(FeatureFlag {ffIdentifier}, description, enabled) ->
                FeatureFlagInfo
                  { ffiName = ffIdentifier,
                    ffiEnabled = enabled,
                    ffiDescription = description
                  }
            )
          <$> featureFlags

isAdminSecretSet :: AuthMode -> Bool
isAdminSecretSet = \case
  AMNoAuth -> False
  _ -> True

isAuthHookSet :: AuthMode -> Bool
isAuthHookSet = \case
  AMAdminSecretAndHook _ _ -> True
  _ -> False

isJWTSet :: AuthMode -> Bool
isJWTSet = \case
  AMAdminSecretAndJWT {} -> True
  _ -> False

getJWTInfo :: AuthMode -> [JWTInfo]
getJWTInfo (AMAdminSecretAndJWT _ jwtCtxs _) =
  let f jwtCtx = case jcxClaims jwtCtx of
        JCNamespace namespace claimsFormat ->
          JWTInfo namespace claimsFormat Nothing
        JCMap claimsMap ->
          JWTInfo (ClaimNs defaultClaimsNamespace) defaultClaimsFormat $ Just claimsMap
   in fmap f jwtCtxs
getJWTInfo _ = mempty
