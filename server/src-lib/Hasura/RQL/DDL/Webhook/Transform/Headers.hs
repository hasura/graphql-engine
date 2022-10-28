{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Network.HTTP.Types qualified as HTTP.Types

-------------------------------------------------------------------------------

-- | The actual header data we are transforming..
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Headers = Headers [HTTP.Types.Header]

instance Transform Headers where
  -- NOTE: GHC does not let us attach Haddock documentation to data family
  -- instances, so 'HeadersTransformFn' is defined separately from this
  -- wrapper.
  newtype TransformFn Headers = HeadersTransformFn_ HeadersTransformFn
    deriving stock (Eq, Generic, Show)
    deriving newtype (Cacheable, NFData, FromJSON, ToJSON)

  newtype TransformCtx Headers = TransformCtx RequestTransformCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyHeadersTransformFn' is defined
  -- separately.
  transform (HeadersTransformFn_ fn) (TransformCtx reqCtx) = applyHeadersTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateHeadersTransformFn' is defined
  -- separately.
  validate engine (HeadersTransformFn_ fn) =
    validateHeadersTransformFn engine fn

-- | The defunctionalized transformation on 'Headers'
newtype HeadersTransformFn
  = -- | Add or replace matching 'HTTP.Types.Header's.
    AddReplaceOrRemove AddReplaceOrRemoveFields
  deriving stock (Eq, Generic, Show)
  deriving newtype (Cacheable, NFData, FromJSON, ToJSON)

-- | The user can supply a set of header keys to be filtered from the
-- request and a set of headers to be added to the request.
data AddReplaceOrRemoveFields = AddReplaceOrRemoveFields
  { -- | A list of key-value pairs for 'HTTP.Types.Header's which
    -- should be added (if they don't exist) or replaced (if they do) within
    -- the HTTP message.
    addOrReplaceHeaders :: [(CI.CI Text, UnescapedTemplate)],
    -- | A list of 'HTTP.Type.Header' keys which should be removed from the
    -- HTTP message.
    removeHeaders :: [CI.CI Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

-- | Provide an implementation for the transformations defined by
-- 'HeadersTransformFn'.
--
-- If one views 'HeadersTransformFn' as an interface describing HTTP message
-- header transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyHeadersTransformFn ::
  MonadError TransformErrorBundle m =>
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
    newHeaders <- liftEither . V.toEither $
      for addOrReplaceHeaders \(rawKey, rawValue) -> do
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

instance FromJSON AddReplaceOrRemoveFields where
  parseJSON = J.withObject "AddReplaceRemoveFields" $ \o -> do
    addOrReplaceHeadersTxt <- o J..:? "add_headers" J..!= mempty
    let addOrReplaceHeaders = M.toList $ mapKeys CI.mk addOrReplaceHeadersTxt

    removeHeadersTxt <- o J..:? "remove_headers" J..!= mempty
    -- NOTE: Ensure that the FromJSON instance is used for deserialization.
    let removeHeaders = coerce @[HeaderKey] removeHeadersTxt

    pure AddReplaceOrRemoveFields {addOrReplaceHeaders, removeHeaders}

instance ToJSON AddReplaceOrRemoveFields where
  toJSON AddReplaceOrRemoveFields {..} =
    J.object
      [ "add_headers" J..= M.fromList (fmap (first CI.original) addOrReplaceHeaders),
        "remove_headers" J..= fmap CI.original removeHeaders
      ]

-- | This newtype exists solely to anchor a `FromJSON` instance and is
-- eliminated in the `TransformHeaders` `FromJSON` instance.
newtype HeaderKey = HeaderKey {unHeaderKey :: CI.CI Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

instance FromJSON HeaderKey where
  parseJSON = J.withText "HeaderKey" \txt -> case CI.mk txt of
    key -> pure $ HeaderKey key
