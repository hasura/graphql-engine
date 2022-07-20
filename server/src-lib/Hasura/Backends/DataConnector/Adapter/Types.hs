{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.Adapter.Types
  ( ConnSourceConfig (..),
    SourceConfig (..),
    DataConnectorBackendConfig,
    DataConnectorName (..),
    DataConnectorOptions (..),
    CountType (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, genericParseJSON, genericToJSON)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.Text.Extended (ToTxt)
import Data.Text.NonEmpty (NonEmptyText)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)
import Witch qualified

data ConnSourceConfig = ConnSourceConfig
  { value :: API.Config,
    template :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData, ToJSON)

-- Default to the old style of ConnSourceConfig if a "value" field isn't present.
-- This will prevent existing configurations from breaking.
-- NOTE: This is planned to be deprecated in future once tooling is migrated.
instance FromJSON ConnSourceConfig where
  parseJSON = J.withObject "ConnSourceConfig" \o ->
    case J.lookup "value" o of
      Just _ -> ConnSourceConfig <$> o J..: "value" <*> o J..:? "template"
      Nothing -> pure $ ConnSourceConfig (API.Config o) Nothing

instance Cacheable ConnSourceConfig where
  unchanged _ = (==)

data SourceConfig = SourceConfig
  { _scEndpoint :: BaseUrl,
    _scConfig :: API.Config,
    _scTemplate :: Maybe Text, -- TODO: Use Parsed Kriti Template
    _scCapabilities :: API.Capabilities,
    _scSchema :: API.SchemaResponse,
    _scManager :: Manager,
    _scDataConnectorName :: DataConnectorName
  }

instance Show SourceConfig where
  show _ = "SourceConfig"

instance J.ToJSON SourceConfig where
  toJSON _ = J.String "SourceConfig"

instance Eq SourceConfig where
  SourceConfig ep1 capabilities1 config1 template1 schema1 _ dcName1 == SourceConfig ep2 capabilities2 config2 template2 schema2 _ dcName2 =
    ep1 == ep2
      && capabilities1 == capabilities2
      && config1 == config2
      && template1 == template2
      && schema1 == schema2
      && dcName1 == dcName2

instance Cacheable SourceConfig where
  unchanged _ = (==)

newtype DataConnectorName = DataConnectorName {unDataConnectorName :: NonEmptyText}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable, ToTxt)
  deriving anyclass (Cacheable, NFData)

instance Witch.From DataConnectorName NonEmptyText

type DataConnectorBackendConfig = InsOrdHashMap DataConnectorName DataConnectorOptions

data DataConnectorOptions = DataConnectorOptions
  {_dcoUri :: BaseUrl}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Cacheable)

instance FromJSON DataConnectorOptions where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DataConnectorOptions where
  toJSON = genericToJSON hasuraJSON

data CountType
  = StarCount
  | ColumnCount (NonEmpty IR.C.Name)
  | ColumnDistinctCount (NonEmpty IR.C.Name)
  deriving (Eq, Ord, Show, Generic, Data)
