{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.DDL.Webhook.Transform.Body
  ( -- * Body Transformations
    Body (..),
    TransformFn (..),

    -- ** Body Transformation Action
    BodyTransformAction (..),
    foldFormEncoded,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Internal.Strict qualified as M
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( RequestTransformCtx (..),
    Template (..),
    TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate,
    runRequestTemplateTransform,
    runUnescapedRequestTemplateTransform',
    validateRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Network.URI.Extended qualified as URI

-------------------------------------------------------------------------------

-- | The actual JSON body we are performing a transformation on.
data Body = JSONBody (Maybe J.Value) | RawBody BL.ByteString
  deriving stock (Eq, Show)

instance Transform Body where
  -- TODO(jkachmar): Document.
  newtype TransformFn Body
    = BodyTransform BodyTransformAction
    deriving stock (Eq, Generic, Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (Cacheable, NFData)

  transform ::
    MonadError TransformErrorBundle m =>
    TransformFn Body ->
    RequestTransformCtx ->
    Body ->
    m Body
  transform (BodyTransform transformation) context _originalBody = do
    case transformation of
      RemoveBody -> pure $ JSONBody Nothing
      ModifyBody template -> do
        result <- liftEither $ runRequestTemplateTransform template context
        pure . JSONBody . Just $ result
      FormUrlEncoded formTemplates -> do
        result <- liftEither . V.toEither $ traverse (runUnescapedRequestTemplateTransform' context) formTemplates
        pure $ RawBody $ foldFormEncoded result

  validate ::
    TemplatingEngine ->
    TransformFn Body ->
    Validation TransformErrorBundle ()
  validate engine = \case
    BodyTransform (RemoveBody) -> pure ()
    BodyTransform (ModifyBody template) -> validateRequestTemplateTransform' engine template
    BodyTransform (FormUrlEncoded kv) -> traverse_ (validateRequestUnescapedTemplateTransform' engine) kv

-- | Helper function for URI escaping 'T.Text' values.
escapeURIText :: T.Text -> T.Text
escapeURIText =
  T.pack . URI.escapeURIString URI.isUnescapedInURIComponent . T.unpack

-- | Helper function for URI escaping 'B.ByteString' values
escapeURIBS :: B.ByteString -> B.ByteString
escapeURIBS =
  TE.encodeUtf8 . T.pack . URI.escapeURIString URI.isUnescapedInURIComponent . T.unpack . TE.decodeUtf8

-- | Fold a 'M.HashMap Text B.ByteString' into a 'BL.ByteString'
-- encoded `x-www-form-urlencoded` request body.
foldFormEncoded :: M.HashMap Text B.ByteString -> BL.ByteString
foldFormEncoded = fold . L.intersperse "&" . M.foldMapWithKey @[BL.ByteString] \k v -> [BL.fromStrict $ TE.encodeUtf8 (escapeURIText k) <> "=" <> escapeURIBS v]

-- | The defunctionalized transformation function on 'Body'.
--
-- Our transformation function can either remove the body or modify it
-- with a 'Template'.
data BodyTransformAction
  = RemoveBody
  | ModifyBody Template
  | FormUrlEncoded (M.HashMap T.Text UnescapedTemplate)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Cacheable, NFData)

instance FromJSON BodyTransformAction where
  parseJSON = J.withObject "BodyTransform" \o -> do
    action <- o J..: "action"
    case (action :: T.Text) of
      "remove" -> pure RemoveBody
      "transform" -> do
        template <- o J..: "template"
        pure $ ModifyBody template
      "x_www_form_urlencoded" -> do
        formTemplates <- o J..: "form_template"
        pure $ FormUrlEncoded formTemplates
      _ -> fail "invalid transform action"

instance ToJSON BodyTransformAction where
  toJSON = \case
    RemoveBody -> J.object ["action" J..= ("remove" :: T.Text)]
    ModifyBody a ->
      J.object
        [ "action" J..= ("transform" :: T.Text),
          "template" J..= J.toJSON a
        ]
    FormUrlEncoded formTemplates ->
      J.object
        [ "action" J..= ("x_www_form_urlencoded" :: T.Text),
          "form_template" J..= J.toJSON formTemplates
        ]
