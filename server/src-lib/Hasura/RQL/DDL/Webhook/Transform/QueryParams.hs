{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.Webhook.Transform.QueryParams
  ( -- * Query transformations
    QueryParams (..),
    TransformFn (..),

    -- ** Method Transformation Action
    QueryParamsTransformAction (..),
  )
where

-------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as M
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( RequestTransformCtx (..),
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate (..),
    validateUnescapedRequestTemplateTransform,
  )
import Network.HTTP.Client.Transformable qualified as HTTP

-------------------------------------------------------------------------------

-- | The actual query params we are transforming
--
-- Necessary boilerplate because otherwise we end up with an
-- orphan instance.
newtype QueryParams = QueryParams {unQueryParams :: HTTP.Query}

instance Transform QueryParams where
  newtype TransformFn QueryParams = QueryParamsTransform QueryParamsTransformAction
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, Cacheable, J.FromJSON, J.ToJSON)

  transform :: MonadError TransformErrorBundle m => TransformFn QueryParams -> RequestTransformCtx -> QueryParams -> m QueryParams
  transform (QueryParamsTransform transformation) context _ = do
    case transformation of
      QueryParamsTransformAction replacements -> do
        -- NOTE: We use `ApplicativeDo` here to take advantage of
        -- Validation's applicative sequencing.
        queryParams <- liftEither . V.toEither $ for replacements \(rawKey, rawValue) -> do
          key <- validateUnescapedRequestTemplateTransform context rawKey
          value <- traverse (validateUnescapedRequestTemplateTransform context) rawValue
          pure (key, value)
        pure $ QueryParams queryParams

-- | The defunctionalized transformation 'QueryParams'
newtype QueryParamsTransformAction = QueryParamsTransformAction [(UnescapedTemplate, Maybe UnescapedTemplate)]
  deriving stock (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON QueryParamsTransformAction where
  toJSON (QueryParamsTransformAction qs) = J.toJSON $ M.fromList qs

instance J.FromJSON QueryParamsTransformAction where
  parseJSON v = QueryParamsTransformAction . M.toList <$> J.parseJSON v
