{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

--
module Hasura.Backends.DataWrapper.API.V0.Scalar.Type
  ( Type (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

data Type
  = StringTy
  | NumberTy
  | BoolTy
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Type

instance HasCodec Type where
  codec =
    named "Type" $
      disjointStringConstCodec [(StringTy, "string"), (NumberTy, "number"), (BoolTy, "bool")]
