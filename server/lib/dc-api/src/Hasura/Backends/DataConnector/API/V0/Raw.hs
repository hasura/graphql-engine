{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Raw
  ( RawRequest (..),
    RawResponse (..),
    rrQuery,
    rrRows,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

-- | A serializable request to retrieve structured data from some
-- source.
data RawRequest = RawRequest
  { _rrQuery :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RawRequest

instance HasCodec RawRequest where
  codec =
    object "RawRequest" $
      RawRequest
        <$> requiredField "query" "A string representing a raw query" .= _rrQuery

-- | The resolved query response provided by the 'POST /raw'
-- endpoint encoded as a list of JSON objects.
data RawResponse = RawResponse
  { _rrRows :: [HashMap.HashMap Text Value]
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec RawResponse

instance HasCodec RawResponse where
  codec =
    named "RawResponse" . object "RawResponse" $
      RawResponse
        <$> requiredField "rows" "The rows returned by the raw query." .= _rrRows

$(makeLenses ''RawRequest)
$(makeLenses ''RawResponse)
