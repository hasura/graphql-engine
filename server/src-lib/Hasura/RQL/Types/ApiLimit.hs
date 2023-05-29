module Hasura.RQL.Types.ApiLimit
  ( ApiLimit (..),
    DepthLimit,
    Limit (..),
    MaxDepth (..),
    MaxNodes (..),
    MaxTime (..),
    MaxBatchSize (..),
    NodeLimit,
    RateLimit,
    RateLimitConfig (..),
    TimeLimit,
    BatchLimit,
    UniqueParamConfig (..),
    emptyApiLimit,
  )
where

import Autodocodec
  ( HasCodec (codec),
    bimapCodec,
    dimapCodec,
    disjointEitherCodec,
    literalTextValueCodec,
    optionalField,
    optionalField',
    optionalFieldWithDefault',
    requiredField',
    requiredFieldWith',
  )
import Autodocodec qualified as AC
import Autodocodec.Extended (integralWithLowerBoundCodec, realFracWithLowerBoundCodec, typeableName)
import Control.Lens
import Data.Aeson
import Data.Aeson.Casing qualified as Casing
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text.Extended (ToTxt (..))
import Data.Typeable (Typeable)
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Session (isSessionVariable)

data ApiLimit = ApiLimit
  { _alRateLimit :: Maybe RateLimit,
    _alDepthLimit :: Maybe DepthLimit,
    _alNodeLimit :: Maybe NodeLimit,
    _alTimeLimit :: Maybe TimeLimit,
    _alBatchLimit :: Maybe BatchLimit,
    _alDisabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance HasCodec ApiLimit where
  codec =
    AC.object "ApiLimit"
      $ ApiLimit
      <$> optionalField' "rate_limit"
      AC..= _alRateLimit
        <*> optionalField' "depth_limit"
      AC..= _alDepthLimit
        <*> optionalField' "node_limit"
      AC..= _alNodeLimit
        <*> optionalField' "time_limit"
      AC..= _alTimeLimit
        <*> optionalField' "batch_limit"
      AC..= _alBatchLimit
        <*> optionalFieldWithDefault' "disabled" False
      AC..= _alDisabled

instance FromJSON ApiLimit where
  parseJSON = withObject "ApiLimit" $ \o ->
    ApiLimit
      <$> o
      .:? "rate_limit"
      <*> o
      .:? "depth_limit"
      <*> o
      .:? "node_limit"
      <*> o
      .:? "time_limit"
      <*> o
      .:? "batch_limit"
      <*> o
      .:? "disabled"
      .!= False

instance ToJSON ApiLimit where
  toJSON =
    genericToJSON (Casing.aesonPrefix Casing.snakeCase) {omitNothingFields = True}

emptyApiLimit :: ApiLimit
emptyApiLimit = ApiLimit Nothing Nothing Nothing Nothing Nothing False

data Limit a = Limit
  { _lGlobal :: a,
    _lPerRole :: Map RoleName a
  }
  deriving (Show, Eq, Generic)

instance (HasCodec a, Typeable a) => HasCodec (Limit a) where
  codec =
    AC.object ("Limit_" <> typeableName @a)
      $ Limit
      <$> requiredField' "global"
      AC..= _lGlobal
        <*> optionalFieldWithDefault' "per_role" mempty
      AC..= _lPerRole

instance (FromJSON a) => FromJSON (Limit a) where
  parseJSON = withObject "Limit" $ \o ->
    Limit <$> o .: "global" <*> o .:? "per_role" .!= mempty

instance (ToJSON a) => ToJSON (Limit a) where
  toJSON =
    genericToJSON (Casing.aesonPrefix Casing.snakeCase)

type RateLimit = Limit RateLimitConfig

type DepthLimit = Limit MaxDepth

type NodeLimit = Limit MaxNodes

type TimeLimit = Limit MaxTime

type BatchLimit = Limit MaxBatchSize

data RateLimitConfig = RateLimitConfig
  { _rlcMaxReqsPerMin :: Int,
    _rlcUniqueParams :: Maybe UniqueParamConfig
  }
  deriving (Show, Eq, Generic)

instance HasCodec RateLimitConfig where
  codec =
    AC.object "RateLimitConfig"
      $ RateLimitConfig
      <$> requiredFieldWith' "max_reqs_per_min" (integralWithLowerBoundCodec 0)
      AC..= _rlcMaxReqsPerMin
        <*> optionalField "unique_params" "This would be either fixed value \"IP\" or a list of Session variables"
      AC..= _rlcUniqueParams

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

instance HasCodec UniqueParamConfig where
  codec = bimapCodec dec enc $ disjointEitherCodec ipAddress sessionVariables
    where
      ipAddress =
        dimapCodec fromEither Left
          $ disjointEitherCodec
            (literalTextValueCodec () "IP")
            (literalTextValueCodec () "ip")
      sessionVariables = codec

      dec (Left _) = Right UPCIpAddress
      dec (Right xs) = UPCSessionVar <$> traverse parseSessVar xs

      parseSessVar s
        | isSessionVariable s && s /= "x-hasura-role" = Right s
        | otherwise = Left "Not a valid value. Should be either: 'IP' or a list of Hasura session variables"

      enc UPCIpAddress = Left ()
      enc (UPCSessionVar xs) = Right xs

      fromEither = either id id

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

instance HasCodec MaxDepth where
  codec =
    dimapCodec MaxDepth unMaxDepth
      $ integralWithLowerBoundCodec 0

newtype MaxNodes = MaxNodes {unMaxNodes :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

instance HasCodec MaxNodes where
  codec =
    dimapCodec MaxNodes unMaxNodes
      $ integralWithLowerBoundCodec 0

newtype MaxTime = MaxTime {unMaxTime :: Seconds}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

instance HasCodec MaxTime where
  codec =
    dimapCodec MaxTime unMaxTime
      $ realFracWithLowerBoundCodec 0

newtype MaxBatchSize = MaxBatchSize {unMaxBatchSize :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

instance HasCodec MaxBatchSize where
  codec =
    dimapCodec MaxBatchSize unMaxBatchSize
      $ integralWithLowerBoundCodec 0

-- | Defers to the (illegal) DiffTime Show instance.
--
-- >>> toTxt (MaxTime 2.5)
-- "2.5s"
instance ToTxt MaxTime where
  toTxt (MaxTime t) = tshow $ seconds t
