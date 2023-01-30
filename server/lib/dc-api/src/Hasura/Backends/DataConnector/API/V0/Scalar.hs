{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataConnector.API.V0.Scalar
  ( ScalarType (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

newtype ScalarType = ScalarType {getScalarType :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving newtype (FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarType

instance HasCodec ScalarType where
  codec =
    named "ScalarType" $
      dimapCodec ScalarType getScalarType textCodec
