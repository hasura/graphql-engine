{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Extended
  (module Network.URI
  )
  where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Hasura.Prelude
import           Network.URI

import qualified Data.Text                  as T

instance {-# INCOHERENT #-} FromJSON URI where
  parseJSON (String uri) = do
    let mUrl = parseURI $ T.unpack uri
    maybe (fail "not a valid URI") return mUrl
  parseJSON _ = fail "not a valid URI"

instance {-# INCOHERENT #-} ToJSON URI where
  toJSON = String . T.pack . show

instance {-# INCOHERENT #-} ToJSONKey URI where
  toJSONKey = toJSONKeyText (T.pack . show)

instance Hashable URI where
  hashWithSalt i = hashWithSalt i . (T.pack . show)
