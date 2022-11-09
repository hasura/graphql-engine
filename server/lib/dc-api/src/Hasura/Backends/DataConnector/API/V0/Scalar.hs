{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

--
module Hasura.Backends.DataConnector.API.V0.Scalar
  ( ScalarType (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey (..), ToJSONKeyFunction (..))
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

data ScalarType
  = StringTy
  | NumberTy
  | BoolTy
  | CustomTy {getCustomTy :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ScalarType

instance ToJSONKey ScalarType where
  toJSONKey = ToJSONKeyText (Key.fromText . scalarTypeToText) (Encoding.text . scalarTypeToText)

instance FromJSONKey ScalarType where
  fromJSONKey = FromJSONKeyText textToScalarType

scalarTypeToText :: ScalarType -> Text
scalarTypeToText = \case
  StringTy -> "string"
  NumberTy -> "number"
  BoolTy -> "bool"
  CustomTy t -> t

textToScalarType :: Text -> ScalarType
textToScalarType t
  | t == "string" = StringTy
  | t == "number" = NumberTy
  | t == "bool" = BoolTy
  | otherwise = CustomTy t

instance HasCodec ScalarType where
  codec =
    named "ScalarType" $
      matchChoiceCodec
        (stringConstCodec [(StringTy, "string"), (NumberTy, "number"), (BoolTy, "bool")])
        (dimapCodec CustomTy getCustomTy textCodec)
        \case
          ty@CustomTy {} -> Right ty
          ty -> Left ty
