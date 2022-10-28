-- | Metadata API Actions relating to Source Kinds
module Hasura.RQL.DDL.SourceKinds
  ( -- * List Source Kinds
    ListSourceKinds (..),
    runListSourceKinds,

    -- * Source Kind Info
    SourceKindInfo (..),
    SourceType (..),

    -- * List Capabilities
    GetSourceKindCapabilities (..),
    runGetSourceKindCapabilities,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (ToTxt (..))
import Data.Text.Extended qualified as Text.E
import Data.Text.NonEmpty (NonEmptyText)
import Data.Text.NonEmpty qualified as NE.Text
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.EncJSON qualified as EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.SchemaCache qualified as SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend qualified as Backend
import Hasura.SQL.BackendMap qualified as BackendMap
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client.Manager qualified as HTTP.Manager

--------------------------------------------------------------------------------

data ListSourceKinds = ListSourceKinds

instance FromJSON ListSourceKinds where
  parseJSON = Aeson.withObject "ListSourceKinds" (const $ pure ListSourceKinds)

instance ToJSON ListSourceKinds where
  toJSON ListSourceKinds = Aeson.object []

--------------------------------------------------------------------------------

data SourceKindInfo = SourceKindInfo
  { _skiSourceKind :: Text,
    _skiBuiltin :: SourceType
  }

instance FromJSON SourceKindInfo where
  parseJSON = Aeson.withObject "SourceKindInfo" \o -> do
    _skiSourceKind <- o .: "kind"
    _skiBuiltin <- o .: "builtin"
    pure SourceKindInfo {..}

instance ToJSON SourceKindInfo where
  toJSON SourceKindInfo {..} = Aeson.object ["kind" .= _skiSourceKind, "builtin" .= _skiBuiltin]

data SourceType = Builtin | Agent

instance FromJSON SourceType where
  parseJSON = Aeson.withBool "source type" \case
    True -> pure Builtin
    False -> pure Agent

instance ToJSON SourceType where
  toJSON Builtin = Aeson.Bool True
  toJSON Agent = Aeson.Bool False

--------------------------------------------------------------------------------

agentSourceKinds :: (Metadata.MetadataM m) => m [SourceKindInfo]
agentSourceKinds = do
  agentsM <- BackendMap.lookup @'Backend.DataConnector . Metadata._metaBackendConfigs <$> Metadata.getMetadata
  case agentsM of
    Nothing -> pure []
    Just (Metadata.BackendConfigWrapper agents) ->
      pure $ fmap mkAgentSource $ InsOrdHashMap.keys agents

mkAgentSource :: DC.Types.DataConnectorName -> SourceKindInfo
mkAgentSource (DC.Types.DataConnectorName name) =
  SourceKindInfo {_skiSourceKind = GQL.unName name, _skiBuiltin = Agent}

mkNativeSource :: Backend.BackendType -> Maybe SourceKindInfo
mkNativeSource = \case
  Backend.DataConnector -> Nothing
  b -> Just $ SourceKindInfo {_skiSourceKind = fromMaybe (toTxt b) (Backend.backendShortName b), _skiBuiltin = Builtin}

runListSourceKinds :: Metadata.MetadataM m => ListSourceKinds -> m EncJSON
runListSourceKinds ListSourceKinds = do
  let builtins = mapMaybe mkNativeSource $ filter (/= Backend.DataConnector) Backend.supportedBackends
  agents <- agentSourceKinds

  pure $ EncJSON.encJFromJValue $ Aeson.object ["sources" .= (builtins <> agents)]

--------------------------------------------------------------------------------

newtype GetSourceKindCapabilities = GetSourceKindCapabilities {_gskcKind :: NonEmptyText}

instance FromJSON GetSourceKindCapabilities where
  parseJSON = Aeson.withObject "GetSourceKindCapabilities" \o -> do
    _gskcKind <- o .: "name"
    pure $ GetSourceKindCapabilities {..}

-- | List Backend Capabilities. Currently this only supports Data Connector Backends.
runGetSourceKindCapabilities ::
  ( HTTP.Manager.HasHttpManagerM m,
    MonadError Error.QErr m,
    SchemaCache.CacheRM m
  ) =>
  GetSourceKindCapabilities ->
  m EncJSON
runGetSourceKindCapabilities GetSourceKindCapabilities {..} = do
  case AB.backendSourceKindFromText $ NE.Text.unNonEmptyText _gskcKind of
    Just backendSourceKind ->
      case AB.unpackAnyBackend @'Backend.DataConnector backendSourceKind of
        Just (Backend.DataConnectorKind dataConnectorName) -> do
          backendCache <- fmap SchemaCache.scBackendCache $ SchemaCache.askSchemaCache
          let capabilitiesMap = maybe mempty SchemaCache.unBackendInfoWrapper $ BackendMap.lookup @'Backend.DataConnector backendCache
          capabilities <-
            HashMap.lookup dataConnectorName capabilitiesMap
              `onNothing` Error.throw400 Error.DataConnectorError ("Source Kind " <> Text.E.toTxt dataConnectorName <> " was not found")

          pure $ EncJSON.encJFromJValue capabilities
        Nothing ->
          -- Must be a native backend
          Error.throw400 Error.DataConnectorError (Text.E.toTxt _gskcKind <> " does not support Capabilities")
    Nothing ->
      Error.throw400 Error.DataConnectorError ("Source Kind " <> Text.E.toTxt _gskcKind <> " was not found")
