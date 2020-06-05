-- | API related to server configuration
module Hasura.Server.API.Config (runGetConfig) where

import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.Prelude
import           Hasura.Server.Auth
import           Hasura.Server.Auth.JWT
import           Hasura.Server.Version                    (HasVersion, Version, currentVersion)

import qualified Hasura.GraphQL.Execute.LiveQuery.Options as LQ

data JWTInfo
  = JWTInfo
  { jwtiClaimsNamespace :: !JWTNamespace
  , jwtiClaimsFormat    :: !JWTClaimsFormat
  , jwtiClaimsMap       :: !(Maybe JWTClaimsMap)
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''JWTInfo)

data ServerConfig
  = ServerConfig
  { scfgVersion            :: !Version
  , scfgIsAdminSecretSet   :: !Bool
  , scfgIsAuthHookSet      :: !Bool
  , scfgIsJwtSet           :: !Bool
  , scfgJwt                :: !(Maybe JWTInfo)
  , scfgIsAllowListEnabled :: !Bool
  , scfgLiveQueries        :: !LQ.LiveQueriesOptions
  , scfgConsoleAssetsDir   :: !(Maybe Text)
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''ServerConfig)

runGetConfig :: HasVersion => AuthMode -> Bool -> LQ.LiveQueriesOptions -> Maybe Text -> ServerConfig
runGetConfig am isAllowListEnabled liveQueryOpts consoleAssetsDir = ServerConfig
    currentVersion
    (isAdminSecretSet am)
    (isAuthHookSet am)
    (isJWTSet am)
    (getJWTInfo am)
    isAllowListEnabled
    liveQueryOpts
    consoleAssetsDir

isAdminSecretSet :: AuthMode -> Bool
isAdminSecretSet = \case
  AMNoAuth -> False
  _        -> True

isAuthHookSet :: AuthMode -> Bool
isAuthHookSet = \case
  AMAdminSecretAndHook _ _ -> True
  _                        -> False

isJWTSet :: AuthMode -> Bool
isJWTSet = \case
  AMAdminSecretAndJWT{} -> True
  _                     -> False

getJWTInfo :: AuthMode -> Maybe JWTInfo
getJWTInfo (AMAdminSecretAndJWT _ jwtCtx _) =
  Just $ case jcxClaims jwtCtx of
    JCNamespace namespace claimsFormat ->
      JWTInfo namespace claimsFormat Nothing
    JCMap claimsMap ->
      JWTInfo (ClaimNs defaultClaimsNamespace) defaultClaimsFormat $ Just claimsMap
getJWTInfo _ = Nothing
