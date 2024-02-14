{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.Types.Webhook.Transform.QueryParams
  ( QueryParams (..),
    QueryParamsTransformFn (..),
    TransformCtx (..),
    TransformFn (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, disjointEitherCodec, hashMapCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (TransformCtx, TransformFn, UnescapedTemplate)
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx)
import Network.HTTP.Client.Transformable qualified as HTTP

-- | The actual query params we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype QueryParams = QueryParams {unQueryParams :: HTTP.Query}

-- | The defunctionalized transformation 'QueryParams'
data QueryParamsTransformFn
  = AddOrReplace [(UnescapedTemplate, Maybe UnescapedTemplate)]
  | ParamTemplate UnescapedTemplate
  deriving (NFData)
  deriving stock (Eq, Generic, Show)

instance HasCodec QueryParamsTransformFn where
  codec = dimapCodec dec enc $ disjointEitherCodec addOrReplaceCodec templateCodec
    where
      addOrReplaceCodec = hashMapCodec (codec @(Maybe UnescapedTemplate))
      templateCodec = codec @UnescapedTemplate

      dec (Left qps) = AddOrReplace $ HashMap.toList qps
      dec (Right template) = ParamTemplate template

      enc (AddOrReplace addOrReplace) = Left $ HashMap.fromList addOrReplace
      enc (ParamTemplate template) = Right template

instance J.ToJSON QueryParamsTransformFn where
  toJSON (AddOrReplace addOrReplace) = J.toJSON $ HashMap.fromList addOrReplace
  toJSON (ParamTemplate template) = J.toJSON template

instance J.FromJSON QueryParamsTransformFn where
  parseJSON xs@(J.Object _) = AddOrReplace . HashMap.toList <$> J.parseJSON xs
  parseJSON xs@(J.String _) = ParamTemplate <$> J.parseJSON xs
  parseJSON _ = fail "Invalid query parameter"

-- NOTE: GHC does not let us attach Haddock documentation to data family
-- instances, so 'QueryParamsTransformFn' is defined separately from this
-- wrapper.
newtype instance TransformFn QueryParams
  = QueryParamsTransformFn_ QueryParamsTransformFn
  deriving stock (Show, Eq, Generic)
  deriving newtype (NFData, FromJSON, ToJSON)

newtype instance TransformCtx QueryParams = TransformCtx RequestTransformCtx
