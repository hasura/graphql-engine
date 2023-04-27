-- | We are in the process of building DTO types incrementally. We use
-- placeholder types in positions in data structures that are not fully-defined
-- yet. For example 'PlaceholderObject' represents some unspecified JSON object,
-- and 'PlaceholderArray' represents an array whose contents are not yet
-- specified.
--
-- We are transitioning from converting 'Hasura.RQL.Types.Metadata' directly to
-- JSON to converting it to 'Hasura.Server.API.DTO.Metadata.MetadataDTO'
-- instead. Serialization and deserialization for placeholder values is
-- delegated to the old JSON serialization code.
module Hasura.Metadata.DTO.Placeholder
  ( PlaceholderArray (..),
    PlaceholderObject (..),
    IsPlaceholder (..),
    placeholderCodecViaJSON,
  )
where

import Autodocodec
  ( Autodocodec,
    HasCodec (codec),
    JSONCodec,
    bimapCodec,
    codecViaAeson,
    dimapCodec,
    valueCodec,
    vectorCodec,
    (<?>),
  )
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as AO
import Data.Aeson.Types qualified as J
import Data.OpenApi qualified as OpenApi
import Data.Vector qualified as V
import Hasura.Prelude

-- TODO: Store ordered aeson values in placeholders instead of stock aeson
-- values so that we can preserve order. We want to do that after #4842 is
-- merged so we can use 'toOrderedJSONVia' to produce the appropriate codecs.

-- | Stands in for an array that we have not had time to fully specify yet.
-- Generated OpenAPI documentation for 'PlaceholderArray' will permit an array
-- of values of any type, and a note will be appended to the documentation
-- string for the value explaining that this is a temporary placeholder.
newtype PlaceholderArray = PlaceholderArray J.Array
  deriving newtype (Show, Eq, FromJSON, ToJSON)
  deriving stock (Generic)
  deriving (OpenApi.ToSchema) via (Autodocodec PlaceholderArray)

-- | Stands in for an object that we have not had time to fully specify yet.
-- Generated OpenAPI documentation for 'PlaceholderObject' will permit an object
-- with any keys with any types of values. A note will be appended to the
-- documentation string for the value explaining that this is a temporary
-- placeholder.
newtype PlaceholderObject = PlaceholderObject J.Object
  deriving newtype (Show, Eq, FromJSON, ToJSON)
  deriving stock (Generic)
  deriving (OpenApi.ToSchema) via (Autodocodec PlaceholderObject)

instance HasCodec PlaceholderArray where
  codec = dimapCodec mapOutput mapInput (vectorCodec valueCodec) <?> documentation
    where
      mapOutput = PlaceholderArray
      mapInput (PlaceholderArray a) = a
      documentation =
        "\n\narray of values of unspecified type - this is a placeholder that will eventually be replaced with a more detailed description"

instance HasCodec PlaceholderObject where
  codec = codecViaAeson "\n\nobject with unspecified properties - this is a placeholder that will eventually be replaced with a more detailed description"

class IsPlaceholder p a | a -> p where
  -- | Use this function to mark an Aeson type (Array or Object) as
  -- a temporary placeholder in a larger data structure.
  placeholder :: a -> p

instance IsPlaceholder PlaceholderArray J.Array where
  placeholder = PlaceholderArray

instance IsPlaceholder PlaceholderObject J.Object where
  placeholder = PlaceholderObject

instance IsPlaceholder PlaceholderArray AO.Array where
  placeholder = PlaceholderArray . V.fromList . map AO.fromOrdered . V.toList

instance IsPlaceholder PlaceholderObject AO.Object where
  placeholder = PlaceholderObject . AO.fromOrderedObject

-- | This placeholder can be used in a codec to represent any type of data that
-- has `FromJSON` and `ToJSON` instances. Generated OpenAPI specifications based
-- on this codec will not show any information about the internal structure of
-- the type so ideally uses of this placeholder should eventually be replaced
-- with more descriptive codecs.
placeholderCodecViaJSON :: (FromJSON a, ToJSON a) => JSONCodec a
placeholderCodecViaJSON =
  bimapCodec dec enc valueCodec
    <?> "value with unspecified type - this is a placeholder that will eventually be replaced with a more detailed description"
  where
    dec = J.parseEither J.parseJSON
    enc = J.toJSON
