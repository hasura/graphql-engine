module Hasura.RQL.Types.HealthCheck
  ( HealthCheckConfig (..),
    HealthCheckTestSql (..),
    HealthCheckInterval (..),
    HealthCheckRetries (..),
    HealthCheckRetryInterval (..),
    HealthCheckTimeout (..),
    defaultHealthCheckTestSql,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Data.Aeson.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude
import Hasura.RQL.Types.Backend

newtype HealthCheckTestSql = HealthCheckTestSql
  { _hctSql :: Text
  }
  deriving (Eq, Generic, Show, Cacheable, Hashable, NFData)

instance ToJSON HealthCheckTestSql where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance FromJSON HealthCheckTestSql where
  parseJSON = withObject "Object" $ \o ->
    HealthCheckTestSql <$> o .:? "sql" .!= defaultTestSql

defaultHealthCheckTestSql :: HealthCheckTestSql
defaultHealthCheckTestSql = HealthCheckTestSql defaultTestSql

defaultTestSql :: Text
defaultTestSql = "SELECT 1"

newtype HealthCheckInterval = HealthCheckInterval {unHealthCheckInterval :: Seconds}
  deriving (Eq, Generic, Show, Cacheable, ToJSON, FromJSON)

instance HasCodec HealthCheckInterval where
  codec = AC.codecViaAeson "HealthCheckInterval"

newtype HealthCheckRetries = HealthCheckRetries {unHealthCheckRetries :: Int}
  deriving (Eq, Generic, Show, Cacheable, FromJSON, ToJSON)

instance HasCodec HealthCheckRetries where
  codec = AC.codecViaAeson "HealthCheckRetries"

newtype HealthCheckRetryInterval = HealthCheckRetryInterval {unHealthCheckRetryInterval :: Seconds}
  deriving (Eq, Generic, Show, Cacheable, ToJSON, FromJSON)

instance HasCodec HealthCheckRetryInterval where
  codec = AC.codecViaAeson "HealthCheckRetryInterval"

newtype HealthCheckTimeout = HealthCheckTimeout {unHealthCheckTimeout :: Seconds}
  deriving (Eq, Generic, Show, Cacheable, FromJSON, ToJSON)

instance HasCodec HealthCheckTimeout where
  codec = AC.codecViaAeson "HealthCheckTimeout"

data HealthCheckConfig b = HealthCheckConfig
  { _hccTest :: HealthCheckTest b,
    _hccInterval :: HealthCheckInterval,
    _hccRetries :: HealthCheckRetries,
    _hccRetryInterval :: HealthCheckRetryInterval,
    _hccTimeout :: HealthCheckTimeout
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (HealthCheckConfig b)

deriving instance (Backend b) => Show (HealthCheckConfig b)

instance (Backend b) => Cacheable (HealthCheckConfig b)

instance (Backend b) => ToJSON (HealthCheckConfig b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => FromJSON (HealthCheckConfig b) where
  parseJSON = withObject "Object" $ \o ->
    HealthCheckConfig
      <$> o .:? "test" .!= defaultHealthCheckTest @b
      <*> o .: "interval"
      <*> o .:? "retries" .!= defaultRetries
      <*> o .:? "retry_interval" .!= defaultRetryInterval
      <*> o .:? "timeout" .!= defaultTimeout

instance (Backend b) => HasCodec (HealthCheckConfig b) where
  codec =
    AC.object "HealthCheckConfig" $
      HealthCheckConfig
        <$> optionalFieldWithOmittedDefaultWith' "test" placeholderCodecViaJSON (defaultHealthCheckTest @b) AC..= _hccTest
        <*> requiredField' "interval" AC..= _hccInterval
        <*> optionalFieldWithOmittedDefault' "retries" defaultRetries AC..= _hccRetries
        <*> optionalFieldWithOmittedDefault' "retry_interval" defaultRetryInterval AC..= _hccRetryInterval
        <*> optionalFieldWithOmittedDefault' "timeout" defaultTimeout AC..= _hccTimeout

defaultRetries :: HealthCheckRetries
defaultRetries = HealthCheckRetries 3

defaultRetryInterval :: HealthCheckRetryInterval
defaultRetryInterval = HealthCheckRetryInterval 10

defaultTimeout :: HealthCheckTimeout
defaultTimeout = HealthCheckTimeout 10
