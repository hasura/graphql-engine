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
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

data Type
  = StringTy
  | NumberTy
  | BoolTy
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass
    ( Cacheable,
      Hashable,
      NFData
    )
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Type

instance HasCodec Type where
  codec =
    named "Type" $
      disjointStringConstCodec [(StringTy, "string"), (NumberTy, "number"), (BoolTy, "bool")]

instance ToTxt Type where
  toTxt = tshow
