{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hasura.Backends.DataConnector.API.V0.Capabilities
  ( Capabilities (..),
    CapabilitiesResponse (..),
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Proxy (..))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject), Referenced (..), Schema (..), ToSchema (..))
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema (ConfigSchemaResponse)
import Prelude

-- | The 'Capabilities' describes the _capabilities_ of the
-- service. Specifically, the service is capable of serving queries
-- which involve relationships.
data Capabilities = Capabilities
  { dcRelationships :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Capabilities

instance HasCodec Capabilities where
  codec =
    object "Capabilities" $
      Capabilities <$> requiredField "relationships" "Does the agent support relationships?" .= dcRelationships

data CapabilitiesResponse = CapabilitiesResponse
  { crCapabilities :: Capabilities,
    crConfigSchemaResponse :: ConfigSchemaResponse
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via Autodocodec CapabilitiesResponse

instance HasCodec CapabilitiesResponse where
  codec =
    object "CapabilitiesResponse" $
      CapabilitiesResponse <$> requiredField "capabilities" "The capabilities of the agent" .= crCapabilities
        <*> requiredField "configSchemas" "The agent's configuration schemas" .= crConfigSchemaResponse

instance ToSchema CapabilitiesResponse where
  declareNamedSchema _ = do
    capabilitiesSchema <- declareNamedSchema (Proxy @Capabilities)
    configSchemasSchema <- declareNamedSchema (Proxy @ConfigSchemaResponse)
    let schema =
          mempty
            { _schemaType = Just OpenApiObject,
              _schemaNullable = Just False,
              _schemaRequired = ["capabilities", "configSchemas"],
              _schemaProperties =
                InsOrdHashMap.fromList
                  [ ("capabilities", Inline $ _namedSchemaSchema capabilitiesSchema),
                    ("configSchemas", Inline $ _namedSchemaSchema configSchemasSchema)
                  ]
            }

    pure $ NamedSchema (Just "CapabilitiesResponse") schema
