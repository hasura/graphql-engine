{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Extended
  ( module Network.URI
  )
  where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Hasura.Prelude
import           Network.URI

import qualified Data.Text        as T

instance {-# INCOHERENT #-} FromJSON URI where
  parseJSON (String uri) = do
    let mUrl = parseURI $ T.unpack uri
    onNothing mUrl (fail "not a valid URI")
  parseJSON _ = fail "not a valid URI"

instance {-# INCOHERENT #-} ToJSON URI where
  toJSON = String . tshow

instance {-# INCOHERENT #-} ToJSONKey URI where
  toJSONKey = toJSONKeyText tshow

instance Hashable URI where
  hashWithSalt i = hashWithSalt i . tshow
