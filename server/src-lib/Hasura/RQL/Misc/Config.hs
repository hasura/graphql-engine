module Hasura.RQL.Misc.Config
  ( GetConfig
  , runGetConfig
  )
  where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Lift)

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Hasura.Server.Auth         as A
import qualified Hasura.Server.Auth.JWT     as J
import qualified Hasura.Server.Version      as V

data GetConfig
  = GetConfig
  deriving (Show, Eq, Lift)

instance FromJSON GetConfig where
  parseJSON _ = return GetConfig

$(deriveToJSON defaultOptions ''GetConfig)

data JWTInfo
  = JWTInfo
  { jwtiClaimNamespace :: !(Maybe Text)
  , jwtiClaimsFormat   :: !J.JWTClaimsFormat
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

runGetConfig :: (UserInfoM m, MonadTx m) => A.AuthMode -> m EncJSON
runGetConfig am = do
  adminOnly
  return $ encJFromJValue $ ServerConfig
    V.currentVersion
    (isAdminSecretSet am)
    (isAuthHookSet am)
    (isJWTSet am)
    (getJWTInfo am)

isAdminSecretSet :: A.AuthMode -> Bool
isAdminSecretSet = \case
  AMNoAuth -> False
  _        -> True

isAuthHookSet :: A.AuthMode -> Bool
isAuthHookSet = \case
  AMAdminSecretAndHook _ _ -> True
  _                        -> False

isJWTSet :: A.AuthMode -> Bool
isJWTSet = \case
  AMAdminSecretAndJWT -> True
  _                   -> False

getJWTInfo :: A.AuthMode -> Maybe JWTInfo
getJWTInfo AMAdminSecretAndJWT _ jwtCtx _ =
  Just $ JWTInfo ns format
  where
    ns = J.jcxClaimNs jwtCtx
    format = J.jcxClaimsFormat jwtCtx
getJWTInfo _ = Nothing
