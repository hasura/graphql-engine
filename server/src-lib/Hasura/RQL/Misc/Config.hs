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
import Hasura.Server.Auth (AuthMode)
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
  { jwtiClaimNamespace :: !Text
  , jwtiClaimsFormat   :: !J.JWTClaimsFormat
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''JWTInfo)

data ServerConfig
  = ServerConfig
  { scfgVersion          :: !Text
  , scfgIsAdminSecretSet :: !Bool
  , scfgIsAccessKeySet   :: !Bool
  , scfgIsAuthHookSet    :: !Bool
  , scfgIsJwtSet         :: !Bool
  , scfgJwt              :: !JWTInfo
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 4 snakeCase) ''ServerConfig)

runGetConfig :: (UserInfoM m, MonadTx m) => AuthMode -> m EncJSON
runGetConfig am = do
  adminOnly
  return $ encJFromJValue $ ServerConfig
    -- TODO(shahidhk): need to populate this object with actual values
    V.currentVersion
    True
    True
    True
    True
    (JWTInfo "ns" J.JCFJson)
