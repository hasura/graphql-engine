-- | This module exports an OpenAPI specification for the GraphQL Engine
-- metadata API.
--
-- The OpenAPI specification for metadata is experimental and incomplete. Please
-- do not incorporate it into essential workflows at this time.
module Hasura.Server.MetadataOpenAPI (metadataOpenAPI) where

import Autodocodec.OpenAPI (declareNamedSchemaViaCodec)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi (Components (..), NamedSchema (..), OpenApi (..))
import Data.OpenApi.Declare (MonadDeclare (declare), runDeclare)
import Data.Proxy (Proxy (..))
import Hasura.Metadata.DTO.Metadata (MetadataDTO)
import Hasura.Prelude

-- | An OpenApi document includes \"schemas\" that describe the data that may be
-- produced or consumed by an API. It can also include \"paths\" which describe
-- REST endpoints, and the document can include other API metadata. This example
-- only includes schemas.
--
-- The OpenAPI specification for metadata is experimental and incomplete. Please
-- do not incorporate it into essential workflows at this time.
metadataOpenAPI :: OpenApi
metadataOpenAPI =
  mempty {_openApiComponents = mempty {_componentsSchemas = definitions}}
  where
    definitions = fst
      $ flip runDeclare mempty
      $ do
        NamedSchema mName codecSchema <- declareNamedSchemaViaCodec (Proxy @MetadataDTO)
        declare $ InsOrdHashMap.fromList [(fromMaybe "MetadataDTO" mName, codecSchema)]
        pure codecSchema
