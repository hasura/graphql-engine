-- | Utility functions for use defining autodocodec codecs.
module Hasura.Metadata.DTO.Utils (versionField, optionalVersionField) where

import Autodocodec
  ( Codec (EqCodec),
    ObjectCodec,
    optionalFieldWith',
    requiredFieldWith',
    scientificCodec,
    (.=),
  )
import Data.Scientific (Scientific)
import Hasura.Prelude

-- | Defines a required object field named @version@ that must have the given
-- integer value. On serialization the field will have the given value
-- automatically. On deserialization parsing will fail unless the field has the
-- exact given value.
versionField :: Integer -> ObjectCodec a Scientific
versionField v = requiredFieldWith' "version" (EqCodec n scientificCodec) .= const n
  where
    n = fromInteger v

-- | Defines an optional object field named @version@ that must have the given
-- integer value if the field is present. On serialization the field will have
-- the given value automatically. On deserialization parsing will fail unless
-- the field has the exact given value, or is absent.
optionalVersionField :: Integer -> ObjectCodec a (Maybe Scientific)
optionalVersionField v =
  optionalFieldWith' "version" (EqCodec n scientificCodec) .= const (Just n)
  where
    n = fromInteger v
