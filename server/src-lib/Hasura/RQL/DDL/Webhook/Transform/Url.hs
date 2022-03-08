{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.DDL.Webhook.Transform.Url
  ( -- * Url Transformations
    Url (..),
    TransformFn (..),

    -- ** Url Transformation Action
    UrlTransformAction (..),
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Text qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( RequestTransformCtx (..),
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate (..),
    mkRequestTemplateTransform,
    throwErrorBundle,
    wrapUnescapedTemplate,
  )
import Network.URI (parseURI)

-------------------------------------------------------------------------------

-- | The actual URL string we are transforming
newtype Url = Url
  { unUrl :: Text
  }
  deriving stock (Eq, Show)

instance Transform Url where
  -- TODO(jkachmar): Document.
  newtype TransformFn Url
    = UrlTransform UrlTransformAction
    deriving stock (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON)
    deriving anyclass (Cacheable, NFData)

  transform ::
    MonadError TransformErrorBundle m =>
    TransformFn Url ->
    RequestTransformCtx ->
    Url ->
    m Url
  transform (UrlTransform transformation) context _originalUrl = do
    case transformation of
      ModifyUrl unescapedTemplate -> do
        let template = wrapUnescapedTemplate unescapedTemplate
        resultJson <- liftEither $ mkRequestTemplateTransform template context
        templatedUrlTxt <- case resultJson of
          J.String templatedUrlTxt -> pure templatedUrlTxt
          val -> do
            let errTxt = "URL Transforms must produce a JSON String: " <> tshow val
            throwErrorBundle errTxt Nothing
        case parseURI (T.unpack templatedUrlTxt) of
          Nothing -> throwErrorBundle ("Invalid URL: " <> templatedUrlTxt) Nothing
          Just _validatedUrl -> pure $ Url templatedUrlTxt

-- | The defunctionalized transformation function on 'Url'
newtype UrlTransformAction
  = ModifyUrl UnescapedTemplate
  deriving newtype (Eq, Generic, Show, FromJSON, ToJSON)
  deriving anyclass (Cacheable, NFData)
