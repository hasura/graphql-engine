{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.RQL.DDL.Webhook.Transform.Headers
  ( -- * Header Transformations
    Headers (..),
    TransformFn (..),

    -- ** Header Transformation Action
    HeadersTransformAction (..),
    ReplaceHeaderFields (..),
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
  ( RequestTransformCtx (..),
    TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate (..),
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Network.HTTP.Types qualified as HTTP.Types

-------------------------------------------------------------------------------

-- | The actual header data we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Headers = Headers [HTTP.Types.Header]

instance Transform Headers where
  newtype TransformFn Headers
    = HeadersTransform HeadersTransformAction
    deriving stock (Eq, Generic, Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (Cacheable, NFData)

  transform ::
    MonadError TransformErrorBundle m =>
    TransformFn Headers ->
    RequestTransformCtx ->
    Headers ->
    m Headers
  transform (HeadersTransform transformation) context (Headers originalHeaders) =
    case transformation of
      ReplaceHeaders ReplaceHeaderFields {rhf_addHeaders, rhf_removeHeaders} -> do
        let removeHeadersBytes = fmap (CI.map TE.encodeUtf8) rhf_removeHeaders
            filteredHeaders =
              originalHeaders & filter \(key, _val) ->
                key `notElem` removeHeadersBytes

        -- NOTE: We use `ApplicativeDo` here to take advantage of
        -- Validation's applicative sequencing.
        newHeaders <- liftEither . V.toEither $ for rhf_addHeaders \(rawKey, rawValue) -> do
          let key = CI.map TE.encodeUtf8 rawKey
          value <- runUnescapedRequestTemplateTransform' context rawValue
          pure (key, value)

        pure . Headers $ filteredHeaders <> newHeaders

  validate ::
    TemplatingEngine ->
    TransformFn Headers ->
    Validation TransformErrorBundle ()
  validate engine (HeadersTransform (ReplaceHeaders (fmap snd . rhf_addHeaders -> templates))) =
    traverse_ (validateRequestUnescapedTemplateTransform' engine) templates

-- | The defunctionalized transformation on 'Headers'
newtype HeadersTransformAction
  = ReplaceHeaders ReplaceHeaderFields
  deriving stock (Generic)
  deriving newtype (Eq, Show, FromJSON, ToJSON)
  deriving anyclass (Cacheable, NFData)

-- | The user can supply a set of header keys to be filtered from the
-- request and a set of headers to be added to the request.
data ReplaceHeaderFields = ReplaceHeaderFields
  { rhf_addHeaders :: [(CI.CI Text, UnescapedTemplate)],
    rhf_removeHeaders :: [CI.CI Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

instance ToJSON ReplaceHeaderFields where
  toJSON ReplaceHeaderFields {..} =
    J.object
      [ "add_headers" J..= M.fromList (fmap (first CI.original) rhf_addHeaders),
        "remove_headers" J..= fmap CI.original rhf_removeHeaders
      ]

instance FromJSON ReplaceHeaderFields where
  parseJSON = J.withObject "ReplaceHeaderFields" $ \o -> do
    addHeaders <- o J..:? "add_headers" J..!= mempty
    let rhf_addHeaders = M.toList $ mapKeys CI.mk addHeaders

    removeHeaders <- o J..:? "remove_headers" J..!= mempty
    -- NOTE: Ensure that the FromJSON instance is used for deserialization.
    let rhf_removeHeaders = coerce @[HeaderKey] removeHeaders

    pure ReplaceHeaderFields {rhf_addHeaders, rhf_removeHeaders}

-- | This newtype exists solely to anchor a `FromJSON` instance and is
-- eliminated in the `TransformHeaders` `FromJSON` instance.
newtype HeaderKey = HeaderKey {unHeaderKey :: CI.CI Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

instance FromJSON HeaderKey where
  parseJSON = J.withText "HeaderKey" \txt -> case CI.mk txt of
    key -> pure $ HeaderKey key
