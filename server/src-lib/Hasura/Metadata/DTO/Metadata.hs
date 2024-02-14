module Hasura.Metadata.DTO.Metadata (MetadataDTO (..)) where

import Autodocodec
  ( Autodocodec (Autodocodec),
    HasCodec (codec),
    dimapCodec,
    disjointEitherCodec,
    named,
    (<?>),
  )
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as OpenApi
import Hasura.Metadata.DTO.MetadataV1 (MetadataV1)
import Hasura.Metadata.DTO.MetadataV2 (MetadataV2)
import Hasura.Metadata.DTO.MetadataV3 (MetadataV3)
import Hasura.Prelude

-- | Exported representation of the GraphQL Engine metadata configuration
-- format.
--
-- The OpenAPI specification for metadata is experimental and incomplete. Please
-- do not incorporate it into essential workflows at this time.
data MetadataDTO = V1 MetadataV1 | V2 MetadataV2 | V3 MetadataV3
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, OpenApi.ToSchema) via (Autodocodec MetadataDTO)

-- | Sum types translate to union types in OpenApi documents via
-- `disjointEitherCodec` (or via `eitherCodec` if the encoded JSON is not
-- necessarily disjoint). In this case we ensure that the encodings of
-- 'MetadataV1', 'MetadataV2', and 'MetadataV3' are disjoint by using
-- 'versionField' in the codec for each variant which injects a field into the
-- JSON representation that requires a specific number to parse successfully.
-- ('MetadataV1' is the only variant where the version field is optional.)
--
-- A codec that represents more than two variants of different types requires
-- nesting `disjointEitherCodec`. Thankfully in the generated OpenApi
-- documentation these three variants are flattened into a single @oneOf@ list
-- of allowed schemas.
instance HasCodec MetadataDTO where
  codec =
    named "Metadata"
      $ dimapCodec decode encode
      $ disjointEitherCodec
        (codec @MetadataV1)
        ( disjointEitherCodec
            (codec @MetadataV2)
            (codec @MetadataV3)
        )
      <?> "configuration format for the Hasura GraphQL Engine"
    where
      decode = either V1 $ either V2 V3
      encode = \case
        V1 v1 -> Left v1
        V2 v2 -> Right $ Left v2
        V3 v3 -> Right $ Right v3
