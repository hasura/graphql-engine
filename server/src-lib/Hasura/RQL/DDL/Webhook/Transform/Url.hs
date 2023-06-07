{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.RQL.DDL.Webhook.Transform.Url
  ( -- * Url Transformations
    Url (..),
    TransformFn (..),
    TransformCtx (..),
    UrlTransformFn (..),
  )
where

-------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Text qualified as T
import Data.Validation
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    throwErrorBundle,
    wrapUnescapedTemplate,
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runRequestTemplateTransform,
    validateRequestUnescapedTemplateTransform',
  )
import Hasura.RQL.Types.Webhook.Transform.Url (TransformCtx (..), TransformFn (..), Url (..), UrlTransformFn (..))
import Network.URI (parseURI)

-------------------------------------------------------------------------------

instance Transform Url where
  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyUrlTransformFn' is defined separately.
  transform (UrlTransformFn_ fn) (TransformCtx reqCtx) = applyUrlTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateUrlTransformFn' is defined separately.
  validate engine (UrlTransformFn_ fn) = validateUrlTransformFn engine fn

-- | Provide an implementation for the transformations defined by
-- 'UrlTransformFn'.
--
-- If one views 'UrlTransformFn' as an interface describing URL
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyUrlTransformFn ::
  (MonadError TransformErrorBundle m) =>
  UrlTransformFn ->
  RequestTransformCtx ->
  Url ->
  m Url
applyUrlTransformFn fn context _oldUrl = case fn of
  Modify unescapedTemplate -> do
    let template = wrapUnescapedTemplate unescapedTemplate
    resultJson <- liftEither $ runRequestTemplateTransform template context
    templatedUrlTxt <- case resultJson of
      J.String templatedUrlTxt -> pure templatedUrlTxt
      val -> do
        let errTxt = "URL Transforms must produce a JSON String: " <> tshow val
        throwErrorBundle errTxt Nothing
    case parseURI (T.unpack templatedUrlTxt) of
      Nothing -> throwErrorBundle ("Invalid URL: " <> templatedUrlTxt) Nothing
      Just _validatedUrl -> pure $ Url templatedUrlTxt

-- | Validate that the provided 'UrlTransformFn' is correct in the context of a
-- particular 'TemplatingEngine'.
--
-- This is a product of the fact that the correctness of a given transformation
-- may be dependent on zero, one, or more of the templated transformations
-- encoded within the given 'UrlTransformFn'.
validateUrlTransformFn ::
  TemplatingEngine ->
  UrlTransformFn ->
  Validation TransformErrorBundle ()
validateUrlTransformFn engine fn = case fn of
  Modify template ->
    validateRequestUnescapedTemplateTransform' engine template
