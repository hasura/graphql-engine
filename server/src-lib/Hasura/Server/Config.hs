module Hasura.Server.Config
  ( runGetConfig
  )
  where

import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.Prelude
import           Hasura.Server.Auth
import           Hasura.Server.Auth.JWT
import           Hasura.Server.Version                    (HasVersion, Version, currentVersion)

import qualified Hasura.GraphQL.Execute.LiveQuery.Options as LQ
import qualified Data.Aeson                               as J
import qualified Data.Parser.JSONPath                     as JSONPath

data JWTInfo
  = JWTInfo
  { jwtiClaimsNamespace     :: !Text
  , jwtiClaimsNamespacePath :: !(Maybe JSONPath.JSONPath)
  , jwtiClaimsFormat        :: !JWTClaimsFormat
  } deriving (Show, Eq)

instance J.ToJSON JWTInfo where
  toJSON (JWTInfo ns (Just nsPath) fmt) =
    J.object [ "claims_namespace" J..= J.String ns
             , "claims_namespace_path" J..= JSONPath.formatPath nsPath
             , "claims_formatPath" J..= fmt
           ]
  toJSON (JWTInfo ns Nothing fmt) =
    J.object [ "claims_namespace" J..= J.String ns
             , "claims_formatPath" J..= fmt
           ]

data ServerConfig
  = ServerConfig
  { scfgVersion            :: !Version
  , scfgIsAdminSecretSet   :: !Bool
  , scfgIsAuthHookSet      :: !Bool
  , scfgIsJwtSet           :: !Bool
  , scfgJwt                :: !(Maybe JWTInfo)
  , scfgIsAllowListEnabled :: !Bool
  , scfgLiveQueries        :: !LQ.LiveQueriesOptions
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''ServerConfig)

runGetConfig :: HasVersion => AuthMode -> Bool -> LQ.LiveQueriesOptions -> ServerConfig
runGetConfig am isAllowListEnabled liveQueryOpts = ServerConfig
    currentVersion
    (isAdminSecretSet am)
    (isAuthHookSet am)
    (isJWTSet am)
    (getJWTInfo am)
    isAllowListEnabled
    liveQueryOpts


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
  case jcxClaimNs jwtCtx of
    ClaimNsPath nsPath -> Just $ JWTInfo "" (Just nsPath) format -- Keeping it empty for backward compatibility
    ClaimNs ns -> Just $ JWTInfo ns Nothing format
  where
    format = jcxClaimsFormat jwtCtx
getJWTInfo _ = Nothing
