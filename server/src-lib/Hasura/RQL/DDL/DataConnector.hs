-- | This module provides operations to load and modify metadata
-- relating to GraphQL Data Connectors.
module Hasura.RQL.DDL.DataConnector
  ( -- * DC Add Agent
    DCAddAgent (..),
    runAddDataConnectorAgent,

    -- * DC Delete Agent
    DCDeleteAgent (..),
    runDeleteDataConnectorAgent,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.NonEmpty (NonEmptyText)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.EncJSON (EncJSON)
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata (MetadataM (putMetadata))
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.SQL.Backend qualified as Backend
import Hasura.SQL.BackendMap qualified as BackendMap
import Servant.Client qualified as Servant

--------------------------------------------------------------------------------

data DCAddAgent = DCAddAgent
  { _gdcaName :: NonEmptyText,
    _gdcaUrl :: Servant.BaseUrl
  }

instance FromJSON DCAddAgent where
  parseJSON = Aeson.withObject "DCAddAgent" \o -> do
    _gdcaName <- o .: "name"
    mUri <- o .: "url"
    case mUri of
      Just _gdcaUrl -> pure DCAddAgent {..}
      Nothing -> fail "Failed to parse Agent URL"

instance ToJSON DCAddAgent where
  toJSON DCAddAgent {..} = Aeson.object ["name" .= _gdcaName, "url" .= show _gdcaUrl]

-- | Insert a new Data Connector Agent into Metadata.
runAddDataConnectorAgent :: (Metadata.MetadataM m) => DCAddAgent -> m EncJSON
runAddDataConnectorAgent DCAddAgent {..} = do
  let kind = DC.Types.DataConnectorName _gdcaName
      agent = DC.Types.DataConnectorOptions _gdcaUrl

  oldMetadata <- Metadata.getMetadata

  let modifiedMetadata =
        oldMetadata & Metadata.metaBackendConfigs %~ BackendMap.modify @'Backend.DataConnector \oldMap ->
          Metadata.BackendConfigWrapper $ InsOrdHashMap.insert kind agent (coerce oldMap)

  putMetadata modifiedMetadata
  pure Common.successMsg

--------------------------------------------------------------------------------

data DCDeleteAgent = DCDeleteAgent {_gdcrName :: NonEmptyText}

instance FromJSON DCDeleteAgent where
  parseJSON = Aeson.withObject "DCDeleteAgent" \o -> do
    _gdcrName <- o .: "name"
    pure $ DCDeleteAgent {..}

instance ToJSON DCDeleteAgent where
  toJSON DCDeleteAgent {..} = Aeson.object ["name" .= _gdcrName]

runDeleteDataConnectorAgent :: (Metadata.MetadataM m) => DCDeleteAgent -> m EncJSON
runDeleteDataConnectorAgent DCDeleteAgent {..} = do
  let kind = DC.Types.DataConnectorName _gdcrName

  oldMetadata <- Metadata.getMetadata

  let modifiedMetadata =
        oldMetadata & Metadata.metaBackendConfigs
          %~ BackendMap.alter @'Backend.DataConnector
            (fmap (coerce . InsOrdHashMap.delete kind . Metadata.unBackendConfigWrapper))

  putMetadata modifiedMetadata
  pure Common.successMsg
