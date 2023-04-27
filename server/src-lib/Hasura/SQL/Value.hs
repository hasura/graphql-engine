module Hasura.SQL.Value
  ( TxtEncodedVal (..),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Types qualified as AT
import Data.Text qualified as T
import Hasura.Prelude

data TxtEncodedVal
  = TENull
  | TELit !T.Text
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedVal

instance J.ToJSON TxtEncodedVal where
  toJSON = \case
    TENull -> AT.Null
    TELit t -> AT.String t

instance J.FromJSON TxtEncodedVal where
  parseJSON J.Null = pure TENull
  parseJSON (J.String t) = pure $ TELit t
  parseJSON v = AT.typeMismatch "String" v
