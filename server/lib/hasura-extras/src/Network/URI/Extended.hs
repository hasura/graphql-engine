{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Extended
  ( module Network.URI,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.Text qualified as T
import Hasura.Prelude
import Network.URI

instance FromJSON URI where
  parseJSON (String uri) = do
    let mUrl = parseURI $ T.unpack uri
    onNothing mUrl (fail "not a valid URI")
  parseJSON _ = fail "not a valid URI"

instance ToJSON URI where
  toJSON = String . tshow

instance ToJSONKey URI where
  toJSONKey = toJSONKeyText tshow

instance Hashable URI where
  hashWithSalt i = hashWithSalt i . tshow
