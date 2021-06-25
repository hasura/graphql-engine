module Hasura.SQL.Value where

import           Hasura.Prelude


import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as AT
import qualified Data.Text        as T

data TxtEncodedVal
  = TENull
  | TELit !T.Text
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedVal

instance A.ToJSON TxtEncodedVal where
  toJSON = \case
    TENull  -> AT.Null
    TELit t -> AT.String t

instance A.FromJSON TxtEncodedVal where
  parseJSON A.Null       = pure TENull
  parseJSON (A.String t) = pure $ TELit t
  parseJSON v            = AT.typeMismatch "String" v
