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

import Control.Monad.Except
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Has
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Monoid
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.API (ErrorResponse (_crDetails))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.EncJSON qualified as EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.SourceKinds
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.SchemaCache.Build qualified as SC.Build
import Hasura.SQL.Backend qualified as Backend
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Tracing (ignoreTraceT)
import Network.HTTP.Client.Manager qualified as HTTP
import Servant.Client qualified as Servant
import Servant.Client.Core.HasClient ((//))
import Servant.Client.Generic (genericClient)

--------------------------------------------------------------------------------

data DCAddAgent = DCAddAgent
  { -- | Source kind, ie., the backend type.
    _gdcaName :: DC.Types.DataConnectorName,
    -- | The Agent URL.
    _gdcaUrl :: Servant.BaseUrl,
    -- | Override the display name provided by the Agent.
    _gdcaDisplayName :: Maybe Text,
    -- | Optionally skip the Agent Validation step.
    _gdcaSkipCheck :: SkipCheck
  }

newtype SkipCheck = SkipCheck Bool
  deriving newtype (FromJSON, ToJSON, Eq)
  deriving (Semigroup, Monoid) via Any

instance FromJSON DCAddAgent where
  parseJSON = Aeson.withObject "DCAddAgent" \o -> do
    _gdcaName <- o .: "name"
    mUri <- o .: "url"
    _gdcaDisplayName <- o .:? "display_name"
    _gdcaSkipCheck <- o .:? "skip_check" .!= SkipCheck False
    case mUri of
      Just _gdcaUrl -> pure DCAddAgent {..}
      Nothing -> fail "Failed to parse Agent URL"

instance ToJSON DCAddAgent where
  toJSON DCAddAgent {..} =
    Aeson.object $
      [ "name" .= _gdcaName,
        "url" .= show _gdcaUrl,
        "skip_check" .= _gdcaSkipCheck
      ]
        ++ ["display_name" .= _gdcaDisplayName | isJust _gdcaDisplayName]

-- | Insert a new Data Connector Agent into Metadata.
runAddDataConnectorAgent ::
  ( Metadata.MetadataM m,
    SC.Build.CacheRWM m,
    Has (L.Logger L.Hasura) r,
    MonadReader r m,
    MonadBaseControl IO m,
    MonadError Error.QErr m,
    MonadIO m,
    HTTP.HasHttpManagerM m
  ) =>
  DCAddAgent ->
  m EncJSON
runAddDataConnectorAgent DCAddAgent {..} = do
  let agent :: DC.Types.DataConnectorOptions
      agent = DC.Types.DataConnectorOptions _gdcaUrl _gdcaDisplayName
  sourceKinds <- (:) "postgres" . fmap _skiSourceKind <$> agentSourceKinds
  if
      | toTxt _gdcaName `elem` sourceKinds -> Error.throw400 Error.AlreadyExists $ "SourceKind '" <> toTxt _gdcaName <> "' already exists."
      | _gdcaSkipCheck == SkipCheck True -> addAgent _gdcaName agent
      | otherwise ->
          checkAgentAvailability _gdcaUrl >>= \case
            NotAvailable err ->
              pure $
                EncJSON.encJFromJValue $
                  Aeson.object
                    [ ("message" .= Aeson.String "Agent is not available"),
                      ("details" .= err)
                    ]
            _ -> addAgent _gdcaName agent

addAgent :: (MonadError Error.QErr m, SC.Build.MetadataM m, SC.Build.CacheRWM m) => DC.Types.DataConnectorName -> DC.Types.DataConnectorOptions -> m EncJSON
addAgent agentName agent = do
  let modifier' =
        Metadata.MetadataModifier $
          Metadata.metaBackendConfigs %~ BackendMap.modify @'Backend.DataConnector \oldMap ->
            Metadata.BackendConfigWrapper $ InsOrdHashMap.insert agentName agent (coerce oldMap)
  SC.Build.withNewInconsistentObjsCheck $ SC.Build.buildSchemaCache modifier'

  pure Common.successMsg

data Availability = Available | NotAvailable Error.QErr

-- | Check DC Agent availability by checking its Capabilities endpoint.
checkAgentAvailability ::
  ( Has (L.Logger L.Hasura) r,
    MonadReader r m,
    MonadIO m,
    MonadBaseControl IO m,
    HTTP.HasHttpManagerM m
  ) =>
  Servant.BaseUrl ->
  m Availability
checkAgentAvailability url = do
  manager <- HTTP.askHttpManager
  logger <- asks getter
  res <- runExceptT $ do
    capabilitiesU <- (ignoreTraceT . flip runAgentClientT (AgentClientContext logger url manager Nothing) $ genericClient @API.Routes // API._capabilities)
    API.capabilitiesCase
      (Error.throw500 "Capabilities request failed unexpectedly")
      pure
      (\e -> Error.throw500WithDetail (API.errorResponseSummary e) (_crDetails e))
      capabilitiesU
  -- NOTE: 'capabilitiesCase' does not handle the 'no connection to host' scenario so we must handle it explicitly here:
  pure (either NotAvailable (const Available) res)

--------------------------------------------------------------------------------

newtype DCDeleteAgent = DCDeleteAgent {_dcdaName :: DC.Types.DataConnectorName}

instance FromJSON DCDeleteAgent where
  parseJSON = Aeson.withObject "DCDeleteAgent" \o -> do
    _dcdaName <- o .: "name"
    pure $ DCDeleteAgent {..}

instance ToJSON DCDeleteAgent where
  toJSON DCDeleteAgent {..} = Aeson.object ["name" .= _dcdaName]

-- | Delete a Data Connector Agent from the Metadata.
runDeleteDataConnectorAgent ::
  ( SC.Build.CacheRWM m,
    Metadata.MetadataM m,
    MonadError Error.QErr m
  ) =>
  DCDeleteAgent ->
  m EncJSON
runDeleteDataConnectorAgent DCDeleteAgent {..} = do
  oldMetadata <- Metadata.getMetadata

  let kindExists = do
        agentMap <- BackendMap.lookup @'Backend.DataConnector $ Metadata._metaBackendConfigs oldMetadata
        InsOrdHashMap.lookup _dcdaName $ Metadata.unBackendConfigWrapper agentMap
  case kindExists of
    Nothing -> Error.throw400 Error.NotFound $ "DC Agent '" <> toTxt _dcdaName <> "' not found"
    Just _ -> do
      let modifier' =
            Metadata.MetadataModifier $
              Metadata.metaBackendConfigs
                %~ BackendMap.alter @'Backend.DataConnector
                  (fmap (coerce . InsOrdHashMap.delete _dcdaName . Metadata.unBackendConfigWrapper))

      SC.Build.withNewInconsistentObjsCheck $ SC.Build.buildSchemaCache modifier'
      pure Common.successMsg
