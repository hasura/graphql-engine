{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Hasura.RQL.Types.Webhook.Transform.Method
  ( Method (..),
    MethodTransformFn (..),
    TransformCtx (..),
    TransformFn (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Autodocodec.Extended (caseInsensitiveTextCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (TransformCtx, TransformFn)
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx)

-- | The actual request method we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Method = Method (CI.CI T.Text)
  deriving stock (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (NFData)

instance HasCodec Method where
  codec = dimapCodec Method coerce caseInsensitiveTextCodec

instance J.ToJSON Method where
  toJSON = J.String . CI.original . coerce

instance J.FromJSON Method where
  parseJSON = J.withText "Method" (pure . coerce . CI.mk)

-- | The defunctionalized transformation on 'Method'.
newtype MethodTransformFn
  = -- | Replace the HTTP existing 'Method' with a new one.
    Replace Method
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

instance HasCodec MethodTransformFn where
  codec = dimapCodec Replace coerce codec

-- NOTE: GHC does not let us attach Haddock documentation to data family
-- instances, so 'MethodTransformFn' is defined separately from this
-- wrapper.
newtype instance TransformFn Method = MethodTransformFn_ MethodTransformFn
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

newtype instance TransformCtx Method = TransformCtx RequestTransformCtx
