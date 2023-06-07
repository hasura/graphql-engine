-- | Metadata API Actions relating to Source Kinds
module Hasura.RQL.DDL.SourceKinds
  ( -- * List Source Kinds
    ListSourceKinds (..),
    runListSourceKinds,
    agentSourceKinds,

    -- * Source Kind Info
    SourceKindInfo (..),
    SourceType (..),
    SourceKinds (..),

    -- * List Capabilities
    GetSourceKindCapabilities (..),
    runGetSourceKindCapabilities,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text.Extended (ToTxt (..))
import Data.Text.Extended qualified as Text.E
import Data.Text.NonEmpty (NonEmptyText)
import Data.Text.NonEmpty qualified as NE.Text
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.EncJSON qualified as EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.BackendType qualified as Backend
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.SchemaCache qualified as SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap qualified as BackendMap
import Language.GraphQL.Draft.Syntax qualified as GQL

--------------------------------------------------------------------------------

data ListSourceKinds = ListSourceKinds

instance FromJSON ListSourceKinds where
  parseJSON = J.withObject "ListSourceKinds" (const $ pure ListSourceKinds)

instance ToJSON ListSourceKinds where
  toJSON ListSourceKinds = J.object []

--------------------------------------------------------------------------------

data SourceKindInfo = SourceKindInfo
  { _skiSourceKind :: Text,
    _skiDisplayName :: Maybe Text,
    _skiReleaseName :: Maybe Text,
    _skiBuiltin :: SourceType,
    _skiAvailable :: Bool
  }

instance FromJSON SourceKindInfo where
  parseJSON = J.withObject "SourceKindInfo" \o -> do
    _skiSourceKind <- o .: "kind"
    _skiDisplayName <- o .:? "display_name"
    _skiReleaseName <- o .:? "release_name"
    _skiBuiltin <- o .: "builtin"
    _skiAvailable <- o .: "available"
    pure SourceKindInfo {..}

instance ToJSON SourceKindInfo where
  toJSON SourceKindInfo {..} =
    J.object
      $ [ "kind" .= _skiSourceKind,
          "builtin" .= _skiBuiltin,
          "available" .= _skiAvailable
        ]
      ++ ["display_name" .= _skiDisplayName | has _skiDisplayName]
      ++ ["release_name" .= _skiReleaseName | has _skiReleaseName]
    where
      has :: Maybe Text -> Bool
      has x = not $ isNothing x || x == Just ""

data SourceType = Builtin | Agent

instance FromJSON SourceType where
  parseJSON = J.withBool "source type" \case
    True -> pure Builtin
    False -> pure Agent

instance ToJSON SourceType where
  toJSON Builtin = J.Bool True
  toJSON Agent = J.Bool False

--------------------------------------------------------------------------------

newtype SourceKinds = SourceKinds {unSourceKinds :: [SourceKindInfo]}
  deriving newtype (Semigroup, Monoid)

instance ToJSON SourceKinds where
  toJSON SourceKinds {..} = J.object ["sources" .= unSourceKinds]

agentSourceKinds :: (Metadata.MetadataM m) => m SourceKinds
agentSourceKinds = do
  agentsM <- BackendMap.lookup @'Backend.DataConnector . Metadata._metaBackendConfigs <$> Metadata.getMetadata
  case agentsM of
    Nothing -> pure mempty
    Just (Metadata.BackendConfigWrapper agents) ->
      pure $ SourceKinds $ fmap mkAgentSource $ Map.toList agents

mkAgentSource :: (DC.Types.DataConnectorName, DC.Types.DataConnectorOptions) -> SourceKindInfo
mkAgentSource (dcName, DC.Types.DataConnectorOptions {_dcoDisplayName}) =
  SourceKindInfo
    { _skiSourceKind = skiKind,
      _skiDisplayName = _dcoDisplayName,
      _skiReleaseName = Nothing,
      _skiBuiltin = Agent,
      _skiAvailable = True
    }
  where
    skiKind = GQL.unName (DC.Types.unDataConnectorName dcName)

mkNativeSource :: Backend.BackendType -> Maybe SourceKindInfo
mkNativeSource = \case
  Backend.DataConnector -> Nothing
  b ->
    Just
      SourceKindInfo
        { _skiSourceKind = fromMaybe (toTxt b) (Backend.backendShortName b),
          _skiBuiltin = Builtin,
          _skiDisplayName = Nothing,
          _skiReleaseName = Nothing,
          _skiAvailable = True
        }

builtinSourceKinds :: SourceKinds
builtinSourceKinds =
  SourceKinds $ mapMaybe mkNativeSource (filter (/= Backend.DataConnector) Backend.supportedBackends)

-- | Collect 'SourceKindInfo' from Native and GDC backend types.
collectSourceKinds :: (Metadata.MetadataM m) => m SourceKinds
collectSourceKinds = fmap (builtinSourceKinds <>) agentSourceKinds

runListSourceKinds ::
  forall m.
  ( Metadata.MetadataM m,
    MonadError Error.QErr m,
    SchemaCache.CacheRM m
  ) =>
  ListSourceKinds ->
  m EncJSON
runListSourceKinds ListSourceKinds = fmap EncJSON.encJFromJValue $ do
  sks <- collectSourceKinds
  fmap SourceKinds $ traverse setNames $ unSourceKinds sks
  where
    setNames :: SourceKindInfo -> m SourceKindInfo
    setNames ski@SourceKindInfo {_skiSourceKind, _skiDisplayName} =
      -- If there are issues fetching the capabilities for an agent, then list it as unavailable.
      flip catchError (const $ pure $ ski {_skiAvailable = False}) do
        ci <- getSourceKindCapabilities ski
        -- Prefer metadata, then capabilities, then source-kind key
        pure
          ski
            { _skiReleaseName = DC.Types._dciReleaseName =<< ci,
              _skiDisplayName = asum [_skiDisplayName, (DC.Types._dciDisplayName =<< ci), Just _skiSourceKind]
            }

    getSourceKindCapabilities :: SourceKindInfo -> m (Maybe DC.Types.DataConnectorInfo)
    getSourceKindCapabilities SourceKindInfo {_skiSourceKind, _skiBuiltin} = case (_skiBuiltin, NE.Text.mkNonEmptyText _skiSourceKind) of
      (Builtin, _) -> pure Nothing
      (Agent, Nothing) -> pure Nothing
      (Agent, Just nesk) -> Just <$> runGetSourceKindCapabilities' (GetSourceKindCapabilities nesk)

--------------------------------------------------------------------------------

newtype GetSourceKindCapabilities = GetSourceKindCapabilities {_gskcKind :: NonEmptyText}

instance FromJSON GetSourceKindCapabilities where
  parseJSON = J.withObject "GetSourceKindCapabilities" \o -> do
    _gskcKind <- o .: "name"
    pure $ GetSourceKindCapabilities {..}

-- | List Backend Capabilities. Currently this only supports Data Connector Backends.
runGetSourceKindCapabilities ::
  ( MonadError Error.QErr m,
    SchemaCache.CacheRM m
  ) =>
  GetSourceKindCapabilities ->
  m EncJSON
runGetSourceKindCapabilities x = EncJSON.encJFromJValue <$> runGetSourceKindCapabilities' x

-- | Main implementation of runGetSourceKindCapabilities that actually returns the DataConnectorInfo
-- and defers json encoding to `runGetSourceKindCapabilities`. This allows reuse and ensures a
-- correct assembly of DataConnectorInfo
runGetSourceKindCapabilities' ::
  ( MonadError Error.QErr m,
    SchemaCache.CacheRM m
  ) =>
  GetSourceKindCapabilities ->
  m DC.Types.DataConnectorInfo
runGetSourceKindCapabilities' GetSourceKindCapabilities {..} = do
  case AB.backendSourceKindFromText $ NE.Text.unNonEmptyText _gskcKind of
    Just backendSourceKind ->
      case AB.unpackAnyBackend @'Backend.DataConnector backendSourceKind of
        Just (Backend.DataConnectorKind dataConnectorName) -> do
          backendCache <- fmap SchemaCache.scBackendCache $ SchemaCache.askSchemaCache
          let capabilitiesMap = maybe mempty SchemaCache.unBackendInfoWrapper $ BackendMap.lookup @'Backend.DataConnector backendCache
          HashMap.lookup dataConnectorName capabilitiesMap
            `onNothing` Error.throw400 Error.DataConnectorError ("Source Kind " <> Text.E.toTxt dataConnectorName <> " was not found")
        Nothing ->
          -- Must be a native backend
          Error.throw400 Error.DataConnectorError (Text.E.toTxt _gskcKind <> " does not support Capabilities")
    Nothing ->
      Error.throw400 Error.DataConnectorError ("Source Kind " <> Text.E.toTxt _gskcKind <> " was not found")
