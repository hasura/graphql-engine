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
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Has
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.DataConnector.Agent.Client qualified as Client
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.EncJSON qualified as EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.SourceKinds
import Hasura.RQL.Types.BackendType qualified as Backend
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.SchemaCache.Build qualified as SC.Build
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Services.Network
import Hasura.Tracing (ignoreTraceT)
import Servant.Client qualified as Servant

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
  parseJSON = J.withObject "DCAddAgent" \o -> do
    _gdcaName <- o .: "name"
    mUri <- o .: "url"
    _gdcaDisplayName <- o .:? "display_name"
    _gdcaSkipCheck <- o .:? "skip_check" .!= SkipCheck False
    case mUri of
      Just _gdcaUrl -> pure DCAddAgent {..}
      Nothing -> fail "Failed to parse Agent URL"

instance ToJSON DCAddAgent where
  toJSON DCAddAgent {..} =
    J.object
      $ [ "name" .= _gdcaName,
          "url" .= show _gdcaUrl,
          "skip_check" .= _gdcaSkipCheck
        ]
      ++ ["display_name" .= _gdcaDisplayName | isJust _gdcaDisplayName]

-- | Insert a new Data Connector Agent into Metadata.
runAddDataConnectorAgent ::
  ( Metadata.MetadataM m,
    ProvidesNetwork m,
    SC.Build.CacheRWM m,
    Has (L.Logger L.Hasura) r,
    MonadReader r m,
    MonadError Error.QErr m,
    MonadIO m,
    MonadBaseControl IO m
  ) =>
  DCAddAgent ->
  m EncJSON
runAddDataConnectorAgent DCAddAgent {..} = do
  let agent :: DC.Types.DataConnectorOptions
      agent = DC.Types.DataConnectorOptions _gdcaUrl _gdcaDisplayName
  sourceKinds <- (:) "postgres" . fmap _skiSourceKind . unSourceKinds <$> agentSourceKinds
  if
    | toTxt _gdcaName `elem` sourceKinds -> Error.throw400 Error.AlreadyExists $ "SourceKind '" <> toTxt _gdcaName <> "' already exists."
    | _gdcaSkipCheck == SkipCheck True -> addAgent _gdcaName agent
    | otherwise ->
        checkAgentAvailability _gdcaUrl >>= \case
          NotAvailable err ->
            pure
              $ EncJSON.encJFromJValue
              $ J.object
                [ ("message" .= J.String "Agent is not available"),
                  ("details" .= err)
                ]
          _ -> addAgent _gdcaName agent

addAgent :: (MonadError Error.QErr m, SC.Build.MetadataM m, SC.Build.CacheRWM m) => DC.Types.DataConnectorName -> DC.Types.DataConnectorOptions -> m EncJSON
addAgent agentName agent = do
  let modifier' =
        Metadata.MetadataModifier
          $ Metadata.metaBackendConfigs
          %~ BackendMap.modify @'Backend.DataConnector \oldMap ->
            Metadata.BackendConfigWrapper $ Map.insert agentName agent (coerce oldMap)
  SC.Build.withNewInconsistentObjsCheck $ SC.Build.buildSchemaCache modifier'

  pure Common.successMsg

data Availability = Available | NotAvailable Error.QErr

-- | Check DC Agent availability by checking its Capabilities endpoint.
checkAgentAvailability ::
  ( ProvidesNetwork m,
    Has (L.Logger L.Hasura) r,
    MonadReader r m,
    MonadIO m,
    MonadBaseControl IO m
  ) =>
  Servant.BaseUrl ->
  m Availability
checkAgentAvailability url = do
  manager <- askHTTPManager
  logger <- asks getter
  res <- runExceptT . ignoreTraceT . flip runAgentClientT (AgentClientContext logger url manager Nothing Nothing) $ Client.capabilities
  -- NOTE: 'capabilitiesCase' does not handle the 'no connection to host' scenario so we must handle it explicitly here:
  pure (either NotAvailable (const Available) res)

--------------------------------------------------------------------------------

newtype DCDeleteAgent = DCDeleteAgent {_dcdaName :: DC.Types.DataConnectorName}

instance FromJSON DCDeleteAgent where
  parseJSON = J.withObject "DCDeleteAgent" \o -> do
    _dcdaName <- o .: "name"
    pure $ DCDeleteAgent {..}

instance ToJSON DCDeleteAgent where
  toJSON DCDeleteAgent {..} = J.object ["name" .= _dcdaName]

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
        Map.lookup _dcdaName $ Metadata.unBackendConfigWrapper agentMap
  case kindExists of
    Nothing -> Error.throw400 Error.NotFound $ "DC Agent '" <> toTxt _dcdaName <> "' not found"
    Just _ -> do
      let modifier' =
            Metadata.MetadataModifier
              $ Metadata.metaBackendConfigs
              %~ BackendMap.alter @'Backend.DataConnector
                (fmap (coerce . Map.delete _dcdaName . Metadata.unBackendConfigWrapper))

      SC.Build.withNewInconsistentObjsCheck $ SC.Build.buildSchemaCache modifier'
      pure Common.successMsg
