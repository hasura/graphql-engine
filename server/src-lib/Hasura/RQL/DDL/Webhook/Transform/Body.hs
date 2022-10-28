{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.DDL.Webhook.Transform.Body
  ( -- * Body Transformations
    Body (..),
    TransformFn (..),
    TransformCtx (..),
    BodyTransformFn (..),
    foldFormEncoded,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Internal.Strict qualified as M
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( Template (..),
    TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate,
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runRequestTemplateTransform,
    runUnescapedRequestTemplateTransform',
    validateRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Network.URI.Extended qualified as URI

-------------------------------------------------------------------------------

-- | HTTP message body being transformed.
data Body
  = JSONBody (Maybe J.Value)
  | RawBody LBS.ByteString
  deriving stock (Eq, Show)

instance Transform Body where
  -- NOTE: GHC does not let us attach Haddock documentation to data family
  -- instances, so 'BodyTransformFn' is defined separately from this wrapper.
  newtype TransformFn Body = BodyTransformFn_ BodyTransformFn
    deriving stock (Eq, Generic, Show)
    deriving newtype (Cacheable, NFData, FromJSON, ToJSON)

  newtype TransformCtx Body = TransformCtx RequestTransformCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyBodyTransformFn' is defined separately.
  transform (BodyTransformFn_ fn) (TransformCtx reqCtx) = applyBodyTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateBodyTransformFn' is defined
  -- separately.
  validate engine (BodyTransformFn_ fn) = validateBodyTransformFn engine fn

-- | The transformations which can be applied to an HTTP message body.
data BodyTransformFn
  = -- | Remove the HTTP message body.
    Remove
  | -- | Modify the JSON message body by applying a 'Template' transformation.
    ModifyAsJSON Template
  | -- | Modify the JSON message body by applying 'UnescapedTemplate'
    -- transformations to each field with a matching 'Text' key.
    ModifyAsFormURLEncoded (M.HashMap Text UnescapedTemplate)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Cacheable, NFData)

-- | Provide an implementation for the transformations defined by
-- 'BodyTransformFn'.
--
-- If one views 'BodyTransformFn' as an interface describing HTTP message body
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyBodyTransformFn ::
  MonadError TransformErrorBundle m =>
  BodyTransformFn ->
  RequestTransformCtx ->
  Body ->
  m Body
applyBodyTransformFn fn context _originalBody = case fn of
  Remove ->
    pure $ JSONBody Nothing
  ModifyAsJSON template -> do
    result <- liftEither $ runRequestTemplateTransform template context
    pure . JSONBody . Just $ result
  ModifyAsFormURLEncoded formTemplates -> do
    result <-
      liftEither . V.toEither . for formTemplates $
        runUnescapedRequestTemplateTransform' context
    pure . RawBody $ foldFormEncoded result

-- | Validate that the provided 'BodyTransformFn' is correct in the context of
-- a particular 'TemplatingEngine'.
--
-- This is a product of the fact that the correctness of a given transformation
-- may be dependent on zero, one, or more of the templated transformations
-- encoded within the given 'BodyTransformFn'.
validateBodyTransformFn ::
  TemplatingEngine ->
  BodyTransformFn ->
  Validation TransformErrorBundle ()
validateBodyTransformFn engine = \case
  Remove ->
    pure ()
  ModifyAsJSON template ->
    validateRequestTemplateTransform' engine template
  ModifyAsFormURLEncoded templateMap ->
    traverse_ (validateRequestUnescapedTemplateTransform' engine) templateMap

-- | Fold a 'M.HashMap' of header key/value pairs into an
-- @x-www-form-urlencoded@ message body.
foldFormEncoded :: M.HashMap Text ByteString -> LBS.ByteString
foldFormEncoded =
  (fold @[] @LBS.ByteString)
    . L.intersperse "&"
    . M.foldMapWithKey @[LBS.ByteString]
      \k v ->
        [ LBS.fromStrict $
            TE.encodeUtf8 (escapeURIText k)
              <> "="
              <> escapeURIBS v
        ]

-- | URI-escape 'Text' blobs.
escapeURIText :: T.Text -> T.Text
escapeURIText =
  T.pack . URI.escapeURIString URI.isUnescapedInURIComponent . T.unpack

-- | URI-escape 'ByteString' blobs, which are presumed to represent 'Text'.
--
-- XXX: This function makes internal usage of 'TE.decodeUtf8', which throws an
-- impure exception when the supplied 'ByteString' cannot be decoded into valid
-- UTF8 text!
escapeURIBS :: ByteString -> ByteString
escapeURIBS =
  TE.encodeUtf8
    . T.pack
    . URI.escapeURIString URI.isUnescapedInURIComponent
    . T.unpack
    . TE.decodeUtf8

instance FromJSON BodyTransformFn where
  parseJSON = J.withObject "BodyTransformFn" \o -> do
    action <- o J..: "action"
    case (action :: Text) of
      "remove" -> pure Remove
      "transform" -> do
        template <- o J..: "template"
        pure $ ModifyAsJSON template
      "x_www_form_urlencoded" -> do
        formTemplates <- o J..: "form_template"
        pure $ ModifyAsFormURLEncoded formTemplates
      _ -> fail "invalid transform action"

instance ToJSON BodyTransformFn where
  toJSON = \case
    Remove -> J.object ["action" J..= ("remove" :: Text)]
    ModifyAsJSON a ->
      J.object
        [ "action" J..= ("transform" :: Text),
          "template" J..= J.toJSON a
        ]
    ModifyAsFormURLEncoded formTemplates ->
      J.object
        [ "action" J..= ("x_www_form_urlencoded" :: Text),
          "form_template" J..= J.toJSON formTemplates
        ]
