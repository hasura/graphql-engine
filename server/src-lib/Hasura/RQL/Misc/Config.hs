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
import           Hasura.Server.Auth
import           Hasura.Server.Auth.JWT
import qualified Hasura.Server.Version      as V

data GetConfig
  = GetConfig
  deriving (Show, Eq, Lift)

instance FromJSON GetConfig where
  parseJSON _ = return GetConfig

$(deriveToJSON defaultOptions ''GetConfig)

data JWTInfo
  = JWTInfo
  { jwtiClaimsNamespace :: !Text
  , jwtiClaimsFormat   :: !JWTClaimsFormat
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

runGetConfig :: (UserInfoM m, MonadTx m) => AuthMode -> m EncJSON
runGetConfig am = do
  adminOnly
  return $ encJFromJValue $ ServerConfig
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
