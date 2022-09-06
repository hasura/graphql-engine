{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

--
module Hasura.Backends.DataConnector.API.V0.Scalar.Type
  ( Type (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

data Type
  = StringTy
  | NumberTy
  | BoolTy
  | CustomTy {getCustomTy :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Type

instance HasCodec Type where
  codec =
    named "ScalarType" $
      matchChoiceCodec
        (stringConstCodec [(StringTy, "string"), (NumberTy, "number"), (BoolTy, "bool")])
        (dimapCodec CustomTy getCustomTy textCodec)
        \case
          ty@CustomTy {} -> Right ty
          ty -> Left ty
