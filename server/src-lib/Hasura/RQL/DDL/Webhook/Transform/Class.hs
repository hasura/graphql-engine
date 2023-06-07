{-# LANGUAGE OverloadedLists #-}
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

import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Extra (toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding (encodeUtf8)
import Data.Validation (Validation)
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (Template (..), TemplatingEngine (..), TransformCtx, TransformErrorBundle (..), TransformFn, UnescapedTemplate (..))

-- | 'Transform' describes how to reify a defunctionalized transformation for
-- a particular request field.
class Transform a where
  -- | 'transform' is a function which takes 'TransformFn' of @a@ and reifies
  -- it into a function of the form:
  --
  -- @
  --   ReqTransformCtx -> a -> m a
  -- @
  transform ::
    (MonadError TransformErrorBundle m) =>
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

-- | A helper function for serializing transformation errors to JSON.
throwErrorBundle ::
  (MonadError TransformErrorBundle m) =>
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

-- | Wrap an 'UnescapedTemplate' with escaped double quotes.
wrapUnescapedTemplate :: UnescapedTemplate -> Template
wrapUnescapedTemplate (UnescapedTemplate txt) = Template $ "\"" <> txt <> "\""

-------------------------------------------------------------------------------
-- Utility functions.

-- | Encode a JSON Scalar Value as a 'ByteString'.
-- If a non-Scalar value is provided, will return a 'TrnasformErrorBundle'
encodeScalar ::
  (MonadError TransformErrorBundle m) =>
  J.Value ->
  m ByteString
encodeScalar = \case
  J.String str -> pure $ encodeUtf8 str
  J.Number num ->
    -- like toLazyByteString, but tuned for output and for common small size:
    pure . LBS.toStrict . toLazyByteStringWith (untrimmedStrategy 24 1024) "" $ scientificBuilder num
  J.Bool True -> pure "true"
  J.Bool False -> pure "false"
  val ->
    throwErrorBundle "Template must produce a String, Number, or Boolean value" (Just val)
