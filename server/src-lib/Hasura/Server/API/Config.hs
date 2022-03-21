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
import Hasura.Prelude
import Hasura.RQL.Types
  ( FunctionPermissionsCtx,
    RemoteSchemaPermsCtx,
  )
import Hasura.Server.Auth
import Hasura.Server.Auth.JWT
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
    scfgIsFunctionPermissionsInferred :: !FunctionPermissionsCtx,
    scfgIsRemoteSchemaPermissionsEnabled :: !RemoteSchemaPermsCtx,
    scfgIsAdminSecretSet :: !Bool,
    scfgIsAuthHookSet :: !Bool,
    scfgIsJwtSet :: !Bool,
    scfgJwt :: ![JWTInfo],
    scfgIsAllowListEnabled :: !Bool,
    scfgLiveQueries :: !ES.LiveQueriesOptions,
    scfgConsoleAssetsDir :: !(Maybe Text),
    scfgExperimentalFeatures :: !(Set.HashSet ExperimentalFeature)
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''ServerConfig)

runGetConfig ::
  FunctionPermissionsCtx ->
  RemoteSchemaPermsCtx ->
  AuthMode ->
  Bool ->
  ES.LiveQueriesOptions ->
  Maybe Text ->
  Set.HashSet ExperimentalFeature ->
  ServerConfig
runGetConfig
  functionPermsCtx
  remoteSchemaPermsCtx
  am
  isAllowListEnabled
  liveQueryOpts
  consoleAssetsDir
  experimentalFeatures =
    ServerConfig
      currentVersion
      functionPermsCtx
      remoteSchemaPermsCtx
      (isAdminSecretSet am)
      (isAuthHookSet am)
      (isJWTSet am)
      (getJWTInfo am)
      isAllowListEnabled
      liveQueryOpts
      consoleAssetsDir
      experimentalFeatures

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
