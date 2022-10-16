{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'Transform' typeclass with various types and helper functions
-- for evaluating transformations.
module Hasura.RQL.DDL.Webhook.Transform.Class
  ( -- * Transformation Interface and Utilities
    Transform (..),

    -- ** Error Context
    TransformErrorBundle (..),
    throwErrorBundle,

    -- * Templating
    TemplatingEngine (..),
    Template (..),

    -- * Unescaped
    UnescapedTemplate (..),
    wrapUnescapedTemplate,
    encodeScalar,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Constraint, Type)
import Data.Text.Encoding (encodeUtf8)
import Data.Validation (Validation)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

-------------------------------------------------------------------------------

-- | 'Transform' describes how to reify a defunctionalized transformation for
-- a particular request field.
type Transform :: Type -> Constraint
class Transform a where
  -- | The associated type 'TransformFn a' is the defunctionalized version
  -- of some transformation that should be applied to a given request field.
  --
  -- In most cases it is some variation on a piece of template text describing
  -- the transformation.
  data TransformFn a :: Type

  data TransformCtx a :: Type

  -- | 'transform' is a function which takes 'TransformFn' of @a@ and reifies
  -- it into a function of the form:
  --
  -- @
  --   ReqTransformCtx -> a -> m a
  -- @
  transform ::
    MonadError TransformErrorBundle m =>
    TransformFn a ->
    TransformCtx a ->
    a ->
    m a

  -- | Validate a 'TransformFn' of @a@.
  validate ::
    TemplatingEngine ->
    TransformFn a ->
    Validation TransformErrorBundle ()

-------------------------------------------------------------------------------

-- | We use collect all transformation failures as a '[J.Value]'.
newtype TransformErrorBundle = TransformErrorBundle
  { tebMessages :: [J.Value]
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup, FromJSON, ToJSON)
  deriving anyclass (Cacheable, NFData)

-- | A helper function for serializing transformation errors to JSON.
throwErrorBundle ::
  MonadError TransformErrorBundle m =>
  Text ->
  Maybe J.Value ->
  m a
throwErrorBundle msg val = do
  let requiredCtx =
        [ "error_code" J..= ("TransformationError" :: Text),
          "message" J..= msg
        ]
      optionalCtx =
        [ ("value" J..=) <$> val
        ]
      err = J.object (requiredCtx <> catMaybes optionalCtx)
  throwError $ TransformErrorBundle [err]

-------------------------------------------------------------------------------

-- | Available templating engines.
data TemplatingEngine
  = Kriti
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (Cacheable, NFData)

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance FromJSON TemplatingEngine where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance ToJSON TemplatingEngine where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

  toEncoding =
    J.genericToEncoding
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

-- | Textual transformation template.
newtype Template = Template
  { unTemplate :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)
  deriving anyclass (Cacheable, NFData)

instance J.FromJSON Template where
  parseJSON = J.withText "Template" (pure . Template)

instance J.ToJSON Template where
  toJSON = J.String . coerce

-------------------------------------------------------------------------------

-- | Validated textual transformation template /for string
-- interpolation only/.
--
-- This is necessary due to Kriti not distinguishing between string
-- literals and string templates.
newtype UnescapedTemplate = UnescapedTemplate
  { getUnescapedTemplate :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)
  deriving anyclass (Cacheable, NFData)

instance J.FromJSON UnescapedTemplate where
  parseJSON = J.withText "Template" (pure . UnescapedTemplate)

instance J.ToJSON UnescapedTemplate where
  toJSON = J.String . coerce

-- | Wrap an 'UnescapedTemplate' with escaped double quotes.
wrapUnescapedTemplate :: UnescapedTemplate -> Template
wrapUnescapedTemplate (UnescapedTemplate txt) = Template $ "\"" <> txt <> "\""

-------------------------------------------------------------------------------
-- Utility functions.

-- | Encode a JSON Scalar Value as a 'ByteString'.
-- If a non-Scalar value is provided, will return a 'TrnasformErrorBundle'
encodeScalar ::
  MonadError TransformErrorBundle m =>
  J.Value ->
  m ByteString
encodeScalar = \case
  J.String str -> pure $ encodeUtf8 str
  J.Number num ->
    pure . LBS.toStrict . toLazyByteString $ scientificBuilder num
  J.Bool True -> pure "true"
  J.Bool False -> pure "false"
  val ->
    throwErrorBundle "Template must produce a String, Number, or Boolean value" (Just val)
