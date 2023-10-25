{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.InterpolatedQuery
  ( InterpolatedQueries (..),
    InterpolatedQuery (..),
    iqId,
    iqItems,
    InterpolatedQueryId (..),
    interpolatedQueryId,
    InterpolatedItem (..),
    _InterpolatedText,
    _InterpolatedScalar,
  )
where

import Autodocodec (discriminatedUnionCodec, mapToDecoder, mapToEncoder, objectCodec, requiredField')
import Autodocodec.Extended (HasCodec (codec), dimapCodec, named, object, requiredField, (.=))
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson qualified as J
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0
import Prelude

-- | Convenience wrapper for set of queries - Useful for Has tuple instances.
newtype InterpolatedQueries = InterpolatedQueries
  {unInterpolatedQueries :: HashMap.HashMap InterpolatedQueryId InterpolatedQuery}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (Semigroup, Monoid)

instance HasCodec InterpolatedQueries where
  codec = named "InterpolatedQueries" $ dimapCodec InterpolatedQueries unInterpolatedQueries codec

data InterpolatedQuery = InterpolatedQuery
  { -- | NOTE: We may not need this in the query itself, could just use the map key, but might be handy to have in-situ for convenience
    _iqId :: InterpolatedQueryId,
    _iqItems :: [InterpolatedItem]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasCodec InterpolatedQuery where
  codec =
    object "InterpolatedQuery" $
      InterpolatedQuery
        <$> requiredField "id" "An id associated with the interpolated query - Should be unique across the request" .= _iqId
        <*> requiredField "items" "Interpolated items in the query" .= _iqItems

-- | Newtype to help keep interpolated IDs distinct from other text
newtype InterpolatedQueryId = InterpolatedQueryId
  {_interpolatedQueryId :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (NFData, Hashable, J.FromJSONKey, J.ToJSONKey)

instance HasCodec InterpolatedQueryId where
  codec = dimapCodec InterpolatedQueryId _interpolatedQueryId codec

data InterpolatedItem
  = InterpolatedText Text
  | InterpolatedScalar API.V0.ScalarValue
  -- TODO: Implement scalar arrays
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasCodec InterpolatedItem where
  codec =
    named "InterpolatedItem" $
      object "InterpolatedItem" $
        discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        InterpolatedText t -> ("text", mapToEncoder t interpolatedTextObjectCodec)
        InterpolatedScalar s -> ("scalar", mapToEncoder s objectCodec)
      dec =
        HashMap.fromList
          [ ("text", ("InterpolatedText", mapToDecoder InterpolatedText interpolatedTextObjectCodec)),
            ("scalar", ("InterpolatedScalar", mapToDecoder InterpolatedScalar objectCodec))
          ]
      interpolatedTextObjectCodec = requiredField' "value"

$(makeLenses ''InterpolatedQueries)
$(makeLenses ''InterpolatedQuery)
$(makeLenses ''InterpolatedQueryId)
$(makePrisms ''InterpolatedItem)
