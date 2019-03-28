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
import qualified Hasura.Server.Auth.JWT     as J

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

runGetConfig :: Monad m => GetConfig -> m EncJSON
runGetConfig _ = undefined
