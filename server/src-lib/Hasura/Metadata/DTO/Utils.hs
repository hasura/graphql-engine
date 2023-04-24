-- | Utility functions for use defining autodocodec codecs.
module Hasura.Metadata.DTO.Utils
  ( boolConstCodec,
    codecNamePrefix,
    discriminatorField,
    discriminatorBoolField,
    fromEnvCodec,
    optionalVersionField,
    versionField,
  )
where

import Autodocodec
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Prelude
import Hasura.SQL.Tag (HasTag (backendTag), reify)

-- | Map a fixed set of two values to boolean values when serializing. The first
-- argument is the value to map to @True@, the second is the value to map to
-- @False@.
boolConstCodec :: Eq a => a -> a -> JSONCodec a
boolConstCodec trueCase falseCase =
  dimapCodec
    (bool trueCase falseCase)
    (== trueCase)
    $ codec @Bool

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

-- | Useful in an object codec for a field that indicates the type of the
-- object within a union. This version assumes that the type of the
-- discriminator field is @Text@.
discriminatorField :: Text -> Text -> ObjectCodec a ()
discriminatorField name value =
  dimapCodec (const ()) (const value) $
    requiredFieldWith' name (literalTextCodec value)

-- | Useful in an object codec for a field that indicates the type of the
-- object within a union. This version assumes that the type of the
-- discriminator field is @Bool@.
discriminatorBoolField :: Text -> Bool -> ObjectCodec a ()
discriminatorBoolField name value =
  dimapCodec (const ()) (const value) $
    requiredFieldWith' name (EqCodec value boolCodec)

-- | Provides a title-cased name for a database kind, inferring the appropriate
-- database kind from type context.
codecNamePrefix :: forall b. (HasTag b) => Text
codecNamePrefix = T.toTitle $ T.toTxt $ reify $ backendTag @b

-- | Represents a text field wrapped in an object with a single property
-- named @from_env@.
--
-- Objects of this form appear in many places in the Metadata API. If we
-- reproduced this codec in each use case the OpenAPI document would have many
-- identical object definitions. Using a shared codec allows a single shared
-- reference.
fromEnvCodec :: JSONCodec Text
fromEnvCodec = object "FromEnv" $ requiredField' "from_env"
