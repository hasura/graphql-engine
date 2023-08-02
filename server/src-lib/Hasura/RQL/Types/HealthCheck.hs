module Hasura.RQL.Types.HealthCheck
  ( HealthCheckConfig (..),
    HealthCheckTestSql (..),
    HealthCheckInterval (..),
    HealthCheckRetries (..),
    HealthCheckRetryInterval (..),
    HealthCheckTimeout (..),
    defaultHealthCheckTestSql,
    healthCheckConfigCodec,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Data.Aeson.Extended
import Data.Aeson.Types (parseFail)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation (HealthCheckImplementation, _hciDefaultTest, _hciTestCodec))

newtype HealthCheckTestSql = HealthCheckTestSql
  { _hctSql :: Text
  }
  deriving (Eq, Generic, Show, Hashable, NFData)

instance HasCodec HealthCheckTestSql where
  codec =
    AC.object "HealthCheckTestSql"
      $ HealthCheckTestSql
      <$> optionalFieldWithDefault' "sql" defaultTestSql
      AC..= _hctSql

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
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance HasCodec HealthCheckInterval where
  codec = dimapCodec HealthCheckInterval unHealthCheckInterval codec

newtype HealthCheckRetries = HealthCheckRetries {unHealthCheckRetries :: Int}
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance HasCodec HealthCheckRetries where
  codec = dimapCodec HealthCheckRetries unHealthCheckRetries codec

newtype HealthCheckRetryInterval = HealthCheckRetryInterval {unHealthCheckRetryInterval :: Seconds}
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance HasCodec HealthCheckRetryInterval where
  codec = dimapCodec HealthCheckRetryInterval unHealthCheckRetryInterval codec

newtype HealthCheckTimeout = HealthCheckTimeout {unHealthCheckTimeout :: Seconds}
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance HasCodec HealthCheckTimeout where
  codec = dimapCodec HealthCheckTimeout unHealthCheckTimeout codec

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

instance (Backend b) => ToJSON (HealthCheckConfig b) where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}

instance (Backend b) => FromJSON (HealthCheckConfig b) where
  parseJSON = case healthCheckImplementation @b of
    Just (HealthCheckImplementation {..}) ->
      withObject "Object" $ \o ->
        HealthCheckConfig
          <$> o
          .:? "test"
          .!= _hciDefaultTest
          <*> o
          .: "interval"
          <*> o
          .:? "retries"
          .!= defaultRetries
          <*> o
          .:? "retry_interval"
          .!= defaultRetryInterval
          <*> o
          .:? "timeout"
          .!= defaultTimeout
    Nothing -> \_ ->
      parseFail
        "cannot deserialize health check config because backend does not implement health checks"

healthCheckConfigCodec ::
  forall b.
  (Backend b) =>
  HealthCheckImplementation (HealthCheckTest b) ->
  JSONCodec (HealthCheckConfig b)
healthCheckConfigCodec (HealthCheckImplementation {..}) =
  AC.object (backendPrefix @b <> "HealthCheckConfig")
    $ HealthCheckConfig
    <$> optionalFieldWithOmittedDefaultWith' "test" _hciTestCodec _hciDefaultTest
    AC..= _hccTest
      <*> requiredField' "interval"
    AC..= _hccInterval
      <*> optionalFieldWithOmittedDefault' "retries" defaultRetries
    AC..= _hccRetries
      <*> optionalFieldWithOmittedDefault' "retry_interval" defaultRetryInterval
    AC..= _hccRetryInterval
      <*> optionalFieldWithOmittedDefault' "timeout" defaultTimeout
    AC..= _hccTimeout

defaultRetries :: HealthCheckRetries
defaultRetries = HealthCheckRetries 3

defaultRetryInterval :: HealthCheckRetryInterval
defaultRetryInterval = HealthCheckRetryInterval 10

defaultTimeout :: HealthCheckTimeout
defaultTimeout = HealthCheckTimeout 10
