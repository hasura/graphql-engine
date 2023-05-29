{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.RQL.DDL.Webhook.Transform.Headers
  ( -- * Header Transformations
    Headers (..),
    TransformFn (..),
    TransformCtx (..),
    HeadersTransformFn (..),
    AddReplaceOrRemoveFields (..),
  )
where

-------------------------------------------------------------------------------

import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Hasura.RQL.Types.Webhook.Transform.Headers (AddReplaceOrRemoveFields (..), Headers (..), HeadersTransformFn (..), TransformCtx (..), TransformFn (..))

-------------------------------------------------------------------------------

instance Transform Headers where
  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyHeadersTransformFn' is defined
  -- separately.
  transform (HeadersTransformFn_ fn) (TransformCtx reqCtx) = applyHeadersTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateHeadersTransformFn' is defined
  -- separately.
  validate engine (HeadersTransformFn_ fn) =
    validateHeadersTransformFn engine fn

-- | Provide an implementation for the transformations defined by
-- 'HeadersTransformFn'.
--
-- If one views 'HeadersTransformFn' as an interface describing HTTP message
-- header transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyHeadersTransformFn ::
  (MonadError TransformErrorBundle m) =>
  HeadersTransformFn ->
  RequestTransformCtx ->
  Headers ->
  m Headers
applyHeadersTransformFn fn context (Headers originalHeaders) = case fn of
  AddReplaceOrRemove fields -> do
    -- NOTE: 'TE.decodeUtf8' can fail with an impure exception; conversion
    -- to bytes is infallible.
    let AddReplaceOrRemoveFields {addOrReplaceHeaders, removeHeaders} = fields
        removeHeadersBytes = fmap (CI.map TE.encodeUtf8) removeHeaders
        filteredHeaders =
          originalHeaders & filter \(key, _val) ->
            key `notElem` removeHeadersBytes

    -- NOTE: We use `ApplicativeDo` here to take advantage of Validation's
    -- applicative sequencing
    newHeaders <- liftEither
      . V.toEither
      $ for addOrReplaceHeaders \(rawKey, rawValue) -> do
        let key = CI.map TE.encodeUtf8 rawKey
        value <- runUnescapedRequestTemplateTransform' context rawValue
        pure (key, value)

    pure . Headers $ filteredHeaders <> newHeaders

-- | Validate that the provided 'HeadersTransformFn' is correct in the context
-- of a particular 'TemplatingEngine'.
--
-- This is a product of the fact that the correctness of a given transformation
-- may be dependent on zero, one, or more of the templated transformations
-- encoded within the given 'HeadersTransformFn'.
validateHeadersTransformFn ::
  TemplatingEngine ->
  HeadersTransformFn ->
  Validation TransformErrorBundle ()
validateHeadersTransformFn engine = \case
  AddReplaceOrRemove fields -> do
    let templates = fields & addOrReplaceHeaders & map snd
    traverse_ (validateRequestUnescapedTemplateTransform' engine) templates
