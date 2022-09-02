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
import Data.Text.NonEmpty qualified as Text.NE
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.SchemaCache qualified as SchemaCache
import Hasura.RQL.Types.SchemaCache.Build qualified as SC.Build
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
runAddDataConnectorAgent ::
  ( Metadata.MetadataM m,
    SC.Build.CacheRWM m,
    MonadError Error.QErr m
  ) =>
  DCAddAgent ->
  m EncJSON
runAddDataConnectorAgent DCAddAgent {..} = do
  let kind = DC.Types.DataConnectorName _gdcaName
      agent = DC.Types.DataConnectorOptions _gdcaUrl

  oldMetadata <- Metadata.getMetadata

  let modifiedMetadata =
        oldMetadata & Metadata.metaBackendConfigs %~ BackendMap.modify @'Backend.DataConnector \oldMap ->
          Metadata.BackendConfigWrapper $ InsOrdHashMap.insert kind agent (coerce oldMap)

  SC.Build.withNewInconsistentObjsCheck $
    SC.Build.buildSchemaCache $ Metadata.MetadataModifier $ const modifiedMetadata

  pure Common.successMsg

--------------------------------------------------------------------------------

newtype DCDeleteAgent = DCDeleteAgent {_dcdaName :: NonEmptyText}

instance FromJSON DCDeleteAgent where
  parseJSON = Aeson.withObject "DCDeleteAgent" \o -> do
    _dcdaName <- o .: "name"
    pure $ DCDeleteAgent {..}

instance ToJSON DCDeleteAgent where
  toJSON DCDeleteAgent {..} = Aeson.object ["name" .= _dcdaName]

-- | Delete a Data Connector Agent from the Metadata.
runDeleteDataConnectorAgent ::
  ( SchemaCache.CacheRM m,
    SC.Build.CacheRWM m,
    Metadata.MetadataM m,
    MonadError Error.QErr m
  ) =>
  DCDeleteAgent ->
  m EncJSON
runDeleteDataConnectorAgent DCDeleteAgent {..} = do
  let kind = DC.Types.DataConnectorName _dcdaName

  oldMetadata <- Metadata.getMetadata

  let kindExists = do
        agentMap <- BackendMap.lookup @'Backend.DataConnector $ Metadata._metaBackendConfigs oldMetadata
        InsOrdHashMap.lookup kind $ Metadata.unBackendConfigWrapper agentMap
  case kindExists of
    Nothing -> Error.throw400 Error.NotFound $ "DC Agent '" <> Text.NE.unNonEmptyText _dcdaName <> "' not found"
    Just _ -> do
      let modifiedMetadata =
            oldMetadata & Metadata.metaBackendConfigs
              %~ BackendMap.alter @'Backend.DataConnector
                (fmap (coerce . InsOrdHashMap.delete kind . Metadata.unBackendConfigWrapper))

      SC.Build.withNewInconsistentObjsCheck $
        SC.Build.buildSchemaCache $ Metadata.MetadataModifier $ const modifiedMetadata
      pure Common.successMsg
