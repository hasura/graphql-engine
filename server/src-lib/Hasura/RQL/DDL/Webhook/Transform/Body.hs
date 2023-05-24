{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.RQL.DDL.Webhook.Transform.Body
  ( -- * Body Transformations
    Body (..),
    TransformFn (..),
    TransformCtx (..),
    BodyTransformFn (..),
    foldFormEncoded,
    validateBodyTransformFn,
  )
where

-------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Internal.Strict qualified as M
import Data.List qualified as L
import Data.Text qualified as T
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
    runRequestTemplateTransform,
    runUnescapedRequestTemplateTransform',
    validateRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Hasura.RQL.Types.Webhook.Transform.Body (Body (..), BodyTransformFn (..), TransformCtx (..), TransformFn (..))
import Network.URI.Extended qualified as URI

-------------------------------------------------------------------------------

instance Transform Body where
  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyBodyTransformFn' is defined separately.
  transform (BodyTransformFn_ fn) (TransformCtx reqCtx) = applyBodyTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateBodyTransformFn' is defined
  -- separately.
  validate engine (BodyTransformFn_ fn) = validateBodyTransformFn engine fn

-- | Provide an implementation for the transformations defined by
-- 'BodyTransformFn'.
--
-- If one views 'BodyTransformFn' as an interface describing HTTP message body
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyBodyTransformFn ::
  (MonadError TransformErrorBundle m) =>
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
      liftEither
        . V.toEither
        . for formTemplates
        $ runUnescapedRequestTemplateTransform' context
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
        [ LBS.fromStrict
            $ TE.encodeUtf8 (escapeURIText k)
            <> "="
            <> escapeURIBS v
          | v /= "null"
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
