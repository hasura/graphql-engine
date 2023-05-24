{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.Types.Webhook.Transform.Headers
  ( AddReplaceOrRemoveFields (..),
    Headers (..),
    HeadersTransformFn (..),
    TransformCtx (..),
    TransformFn (..),
  )
where

import Autodocodec
import Autodocodec.Extended (caseInsensitiveHashMapCodec, caseInsensitiveTextCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (TransformCtx, TransformFn, UnescapedTemplate (..))
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx)
import Network.HTTP.Types qualified as HTTP.Types

-- | The actual header data we are transforming..
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Headers = Headers [HTTP.Types.Header]

-- | The defunctionalized transformation on 'Headers'
newtype HeadersTransformFn
  = -- | Add or replace matching 'HTTP.Types.Header's.
    AddReplaceOrRemove AddReplaceOrRemoveFields
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

instance HasCodec HeadersTransformFn where
  codec = dimapCodec AddReplaceOrRemove coerce codec

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
  deriving anyclass (NFData)

instance HasCodec AddReplaceOrRemoveFields where
  codec =
    object "AddReplaceOrRemoveFields"
      $ AddReplaceOrRemoveFields
      <$> optionalFieldWithDefaultWith' "add_headers" addCodec mempty
      .= addOrReplaceHeaders
        <*> optionalFieldWithDefaultWith' "remove_headers" removeCodec mempty
      .= removeHeaders
    where
      addCodec = dimapCodec HashMap.toList HashMap.fromList $ caseInsensitiveHashMapCodec codec
      removeCodec = listCodec caseInsensitiveTextCodec

instance FromJSON AddReplaceOrRemoveFields where
  parseJSON = J.withObject "AddReplaceRemoveFields" $ \o -> do
    addOrReplaceHeadersTxt <- o J..:? "add_headers" J..!= mempty
    let addOrReplaceHeaders = HashMap.toList $ mapKeys CI.mk addOrReplaceHeadersTxt

    removeHeadersTxt <- o J..:? "remove_headers" J..!= mempty
    -- NOTE: Ensure that the FromJSON instance is used for deserialization.
    let removeHeaders = coerce @[HeaderKey] removeHeadersTxt

    pure AddReplaceOrRemoveFields {addOrReplaceHeaders, removeHeaders}

instance ToJSON AddReplaceOrRemoveFields where
  toJSON AddReplaceOrRemoveFields {..} =
    J.object
      [ "add_headers" J..= HashMap.fromList (fmap (first CI.original) addOrReplaceHeaders),
        "remove_headers" J..= fmap CI.original removeHeaders
      ]

-- | This newtype exists solely to anchor a `FromJSON` instance and is
-- eliminated in the `TransformHeaders` `FromJSON` instance.
newtype HeaderKey = HeaderKey {unHeaderKey :: CI.CI Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance FromJSON HeaderKey where
  parseJSON = J.withText "HeaderKey" \txt -> case CI.mk txt of
    key -> pure $ HeaderKey key

-- NOTE: GHC does not let us attach Haddock documentation to data family
-- instances, so 'HeadersTransformFn' is defined separately from this
-- wrapper.
newtype instance TransformFn Headers = HeadersTransformFn_ HeadersTransformFn
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

newtype instance TransformCtx Headers = TransformCtx RequestTransformCtx
