{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataConnector.API.V0.Scalar
  ( ScalarType (..),
    ScalarValue (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..), Value)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

newtype ScalarType = ScalarType {getScalarType :: Text}
  deriving stock (Eq, Generic, Ord, Show, Data)
  deriving anyclass (Hashable, NFData)
  deriving newtype (FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarType

instance HasCodec ScalarType where
  codec =
    named "ScalarType" $
      dimapCodec ScalarType getScalarType textCodec

data ScalarValue = ScalarValue
  { _svValue :: Value,
    _svValueType :: ScalarType
  }
  deriving stock (Eq, Generic, Ord, Show, Data)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarValue

instance HasCodec ScalarValue where
  codec = object "ScalarValue" $ objectCodec

instance HasObjectCodec ScalarValue where
  objectCodec =
    ScalarValue
      <$> requiredField' "value" .= _svValue
      <*> requiredField' "value_type" .= _svValueType
