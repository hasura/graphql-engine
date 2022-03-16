{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Scalar.Type
  ( Type (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

data Type
  = StringTy
  | NumberTy
  | BoolTy
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass
    ( Cacheable,
      FromJSON,
      FromJSONKey,
      Hashable,
      NFData,
      ToJSON,
      ToJSONKey
    )

instance ToTxt Type where
  toTxt = tshow
