{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Extended
  ( module Network.URI,
  )
where

import Data.Hashable
import Hasura.Prelude
import Network.URI

-- -- NOTE: in aeson 2.2.0.0 instances are defined. They seem largely compatible,
-- -- but because the instances here are undocumented we can't know whether any
-- -- differences (e.g. the error string here) are significant, intended, or
-- -- merely incidental.
--
-- instance FromJSON URI where
--   parseJSON (String uri) = do
--     let mUrl = parseURI $ T.unpack uri
--     onNothing mUrl (fail "not a valid URI")
--   parseJSON _ = fail "not a valid URI"

-- instance ToJSON URI where
--   toJSON = String . tshow

-- instance ToJSONKey URI where
--   toJSONKey = toJSONKeyText tshow

instance Hashable URI where
  hashWithSalt i = hashWithSalt i . tshow
