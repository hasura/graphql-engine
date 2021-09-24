module Hasura.SQL.Value where

import Data.Aeson qualified as A
import Data.Aeson.Types qualified as AT
import Data.Text qualified as T
import Hasura.Prelude

data TxtEncodedVal
  = TENull
  | TELit !T.Text
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedVal

instance A.ToJSON TxtEncodedVal where
  toJSON = \case
    TENull -> AT.Null
    TELit t -> AT.String t

instance A.FromJSON TxtEncodedVal where
  parseJSON A.Null = pure TENull
  parseJSON (A.String t) = pure $ TELit t
  parseJSON v = AT.typeMismatch "String" v
