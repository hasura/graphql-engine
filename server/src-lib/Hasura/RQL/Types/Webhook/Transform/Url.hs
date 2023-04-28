module Hasura.RQL.Types.Webhook.Transform.Url
  ( Url (..),
    UrlTransformFn (..),
    TransformCtx (..),
    TransformFn (..),
  )
where

import Autodocodec (HasCodec, codec, dimapCodec)
import Data.Aeson (FromJSON, ToJSON)
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (TransformCtx, TransformFn, UnescapedTemplate (..))
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx (..))

-- | The actual URL string we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Url = Url {unUrl :: Text}
  deriving stock (Eq, Show)

-- | The defunctionalized transformation function on 'Url'
newtype UrlTransformFn
  = Modify UnescapedTemplate
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

instance HasCodec UrlTransformFn where
  codec = dimapCodec Modify coerce codec

-- NOTE: GHC does not let us attach Haddock documentation to data family
-- instances, so 'UrlTransformFn' is defined separately from this
-- wrapper.
newtype instance TransformFn Url = UrlTransformFn_ UrlTransformFn
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

newtype instance TransformCtx Url = TransformCtx RequestTransformCtx
