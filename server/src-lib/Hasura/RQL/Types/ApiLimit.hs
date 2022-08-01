module Hasura.RQL.Types.ApiLimit
  ( ApiLimit (..),
    DepthLimit,
    Limit (..),
    MaxDepth (..),
    MaxNodes (..),
    MaxTime (..),
    NodeLimit,
    RateLimit,
    RateLimitConfig (..),
    TimeLimit,
    UniqueParamConfig (..),
    emptyApiLimit,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.Casing qualified as Casing
import Data.Text qualified as T
import Data.Text.Extended (ToTxt (..))
import Hasura.Prelude
import Hasura.Server.Utils (isSessionVariable)
import Hasura.Session (RoleName)

data ApiLimit = ApiLimit
  { _alRateLimit :: Maybe RateLimit,
    _alDepthLimit :: Maybe DepthLimit,
    _alNodeLimit :: Maybe NodeLimit,
    _alTimeLimit :: Maybe TimeLimit,
    _alDisabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ApiLimit where
  parseJSON = withObject "ApiLimit" $ \o ->
    ApiLimit
      <$> o .:? "rate_limit"
      <*> o .:? "depth_limit"
      <*> o .:? "node_limit"
      <*> o .:? "time_limit"
      <*> o .:? "disabled" .!= False

instance ToJSON ApiLimit where
  toJSON =
    genericToJSON (Casing.aesonPrefix Casing.snakeCase) {omitNothingFields = True}

emptyApiLimit :: ApiLimit
emptyApiLimit = ApiLimit Nothing Nothing Nothing Nothing False

data Limit a = Limit
  { _lGlobal :: a,
    _lPerRole :: InsOrdHashMap RoleName a
  }
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Limit a) where
  parseJSON = withObject "Limit" $ \o ->
    Limit <$> o .: "global" <*> o .:? "per_role" .!= mempty

instance ToJSON a => ToJSON (Limit a) where
  toJSON =
    genericToJSON (Casing.aesonPrefix Casing.snakeCase)

type RateLimit = Limit RateLimitConfig

type DepthLimit = Limit MaxDepth

type NodeLimit = Limit MaxNodes

type TimeLimit = Limit MaxTime

data RateLimitConfig = RateLimitConfig
  { _rlcMaxReqsPerMin :: Int,
    _rlcUniqueParams :: Maybe UniqueParamConfig
  }
  deriving (Show, Eq, Generic)

instance FromJSON RateLimitConfig where
  parseJSON =
    genericParseJSON (Casing.aesonPrefix Casing.snakeCase)

instance ToJSON RateLimitConfig where
  toJSON =
    genericToJSON (Casing.aesonPrefix Casing.snakeCase)

-- | The unique key using which an authenticated client can be identified
data UniqueParamConfig
  = -- | it can be a list of session variable (like session var in 'UserInfo')
    UPCSessionVar [Text]
  | -- | or it can be an IP address
    UPCIpAddress
  deriving (Show, Eq, Generic)

instance ToJSON UniqueParamConfig where
  toJSON = \case
    UPCSessionVar xs -> toJSON xs
    UPCIpAddress -> "IP"

instance FromJSON UniqueParamConfig where
  parseJSON = \case
    String v -> case T.toLower v of
      "ip" -> pure UPCIpAddress
      _ -> fail errMsg
    Array xs -> traverse parseSessVar xs <&> UPCSessionVar . toList
    _ -> fail errMsg
    where
      parseSessVar = \case
        String s
          | isSessionVariable s && s /= "x-hasura-role" -> pure s
          | otherwise -> fail errMsg
        _ -> fail errMsg
      errMsg = "Not a valid value. Should be either: 'IP' or a list of Hasura session variables"

newtype MaxDepth = MaxDepth {unMaxDepth :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype MaxNodes = MaxNodes {unMaxNodes :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype MaxTime = MaxTime {unMaxTime :: Seconds}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Defers to the (illegal) DiffTime Show instance.
--
-- >>> toTxt (MaxTime 2.5)
-- "2.5s"
instance ToTxt MaxTime where
  toTxt (MaxTime t) = tshow $ seconds t
