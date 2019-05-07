module Hasura.Server.Config
  ( runGetConfig
  )
  where

import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.Prelude
import           Hasura.Server.Auth
import           Hasura.Server.Auth.JWT
import qualified Hasura.Server.Version  as V

data JWTInfo
  = JWTInfo
  { jwtiClaimsNamespace :: !Text
  , jwtiClaimsFormat    :: !JWTClaimsFormat
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''JWTInfo)

data ServerConfig
  = ServerConfig
  { scfgVersion          :: !Text
  , scfgIsAdminSecretSet :: !Bool
  , scfgIsAuthHookSet    :: !Bool
  , scfgIsJwtSet         :: !Bool
  , scfgJwt              :: !(Maybe JWTInfo)
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''ServerConfig)

runGetConfig ::  AuthMode -> ServerConfig
runGetConfig am = ServerConfig
    V.currentVersion
    (isAdminSecretSet am)
    (isAuthHookSet am)
    (isJWTSet am)
    (getJWTInfo am)

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
  Just $ JWTInfo ns format
  where
    ns = fromMaybe defaultClaimNs $ jcxClaimNs jwtCtx
    format = jcxClaimsFormat jwtCtx
getJWTInfo _ = Nothing
