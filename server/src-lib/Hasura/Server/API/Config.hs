{-# LANGUAGE TemplateHaskell #-}

-- | API related to server configuration
module Hasura.Server.API.Config
-- required by pro
  ( ServerConfig (..),
    runGetConfig,
  )
where

import Data.Aeson.TH
import Data.HashSet qualified as Set
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Prelude
import Hasura.Server.Auth
import Hasura.Server.Auth.JWT
import Hasura.Server.Init.Config (API (METRICS), AllowListStatus)
import Hasura.Server.Types (ExperimentalFeature)
import Hasura.Server.Version (Version, currentVersion)

data JWTInfo = JWTInfo
  { jwtiClaimsNamespace :: !JWTNamespace,
    jwtiClaimsFormat :: !JWTClaimsFormat,
    jwtiClaimsMap :: !(Maybe JWTCustomClaimsMap)
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''JWTInfo)

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
    scfgDefaultNamingConvention :: !NamingCase
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''ServerConfig)

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
  defaultNamingConvention =
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
    where
      isPrometheusMetricsEnabled = METRICS `Set.member` enabledAPIs

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
