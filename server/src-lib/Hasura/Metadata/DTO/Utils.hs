-- | Utility functions for use defining autodocodec codecs.
module Hasura.Metadata.DTO.Utils
  ( codecNamePrefix,
    fromEnvCodec,
    versionField,
    optionalVersionField,
    typeableName,
  )
where

import Autodocodec
  ( Codec (EqCodec),
    JSONCodec,
    ObjectCodec,
    object,
    optionalFieldWith',
    requiredField',
    requiredFieldWith',
    scientificCodec,
    (.=),
  )
import Data.Char (isAlphaNum)
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Hasura.Prelude
import Hasura.SQL.Tag (HasTag (backendTag), reify)

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

-- | Provides a title-cased name for a database kind, inferring the appropriate
-- database kind from type context.
codecNamePrefix :: forall b. (HasTag b) => Text
codecNamePrefix = T.toTitle $ T.toTxt $ reify $ backendTag @b

-- | Provides a string based on the given type to use to uniquely name
-- instantiations of polymorphic codecs.
typeableName :: forall a. (Typeable a) => Text
typeableName = T.map toValidChar $ tshow $ typeRep (Proxy @a)
  where
    toValidChar c = if isAlphaNum c then c else '_'

-- | Represents a text field wrapped in an object with a single property
-- named @from_env@.
--
-- Objects of this form appear in many places in the Metadata API. If we
-- reproduced this codec in each use case the OpenAPI document would have many
-- identical object definitions. Using a shared codec allows a single shared
-- reference.
fromEnvCodec :: JSONCodec Text
fromEnvCodec = object "FromEnv" $ requiredField' "from_env"
