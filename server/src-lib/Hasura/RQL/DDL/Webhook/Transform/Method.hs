{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.Webhook.Transform.Method
  ( -- * Method transformations
    Method (..),
    TransformFn (..),

    -- ** Method Transformation Action
    MethodTransformAction (..),
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Validation
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( RequestTransformCtx (..),
    TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
  )

-------------------------------------------------------------------------------

-- | The actual request method we are transforming
newtype Method = Method (CI.CI T.Text)
  deriving stock (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON Method where
  toJSON = J.String . CI.original . coerce

instance J.FromJSON Method where
  parseJSON = J.withText "Method" (pure . coerce . CI.mk)

instance Transform Method where
  newtype TransformFn Method = MethodTransform MethodTransformAction
    deriving stock (Eq, Generic, Show)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass (Cacheable, NFData)

  -- In the case of 'Method' we simply replace the 'Method' with the one in the request transform.
  transform :: MonadError TransformErrorBundle m => TransformFn Method -> RequestTransformCtx -> Method -> m Method
  transform (MethodTransform (ReplaceMethod method)) _ _ = pure method

  -- NOTE: Do we want to validate the method verb?
  validate ::
    TemplatingEngine ->
    TransformFn Method ->
    Validation TransformErrorBundle ()
  validate _ _ = pure ()

-- | The defunctionalized transformation on 'Method'.
--
-- In this case our transformation simply replaces the 'Method' with a
-- new one.
newtype MethodTransformAction = ReplaceMethod Method
  deriving stock (Generic)
  deriving newtype (Eq, Show, FromJSON, ToJSON)
  deriving anyclass (Cacheable, NFData)
