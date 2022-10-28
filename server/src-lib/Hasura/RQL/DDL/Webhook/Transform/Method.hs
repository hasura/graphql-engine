{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.Webhook.Transform.Method
  ( -- * Method transformations
    Method (..),
    TransformFn (..),
    TransformCtx (..),
    MethodTransformFn (..),
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
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request (RequestTransformCtx)

-------------------------------------------------------------------------------

-- | The actual request method we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype Method = Method (CI.CI T.Text)
  deriving stock (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON Method where
  toJSON = J.String . CI.original . coerce

instance J.FromJSON Method where
  parseJSON = J.withText "Method" (pure . coerce . CI.mk)

instance Transform Method where
  -- NOTE: GHC does not let us attach Haddock documentation to data family
  -- instances, so 'MethodTransformFn' is defined separately from this
  -- wrapper.
  newtype TransformFn Method = MethodTransformFn_ MethodTransformFn
    deriving stock (Eq, Generic, Show)
    deriving newtype (Cacheable, NFData, FromJSON, ToJSON)

  newtype TransformCtx Method = TransformCtx RequestTransformCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyMethodTransformFn' is defined
  -- separately.
  transform (MethodTransformFn_ fn) (TransformCtx reqCtx) = applyMethodTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateMethodTransformFn' is defined
  -- separately.
  validate engine (MethodTransformFn_ fn) = validateMethodTransformFn engine fn

-- | The defunctionalized transformation on 'Method'.
newtype MethodTransformFn
  = -- | Replace the HTTP existing 'Method' with a new one.
    Replace Method
  deriving stock (Eq, Generic, Show)
  deriving newtype (Cacheable, NFData, FromJSON, ToJSON)

-- | Provide an implementation for the transformations defined by
-- 'MethodTransformFn'.
--
-- If one views 'MethodTransformFn' as an interface describing HTTP method
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyMethodTransformFn ::
  MonadError TransformErrorBundle m =>
  MethodTransformFn ->
  RequestTransformCtx ->
  Method ->
  m Method
applyMethodTransformFn fn _context _oldMethod = case fn of
  Replace newMethod -> pure newMethod

-- | Validate that the provided 'MethodTransformFn' is correct in the context
-- of a particular 'TemplatingEngine'.
--
-- This is a product of the fact that the correctness of a given transformation
-- may be dependent on zero, one, or more of the templated transformations
-- encoded within the given 'MethodTransformFn'.
--
-- XXX: Do we want to validate the HTTP method verb?
validateMethodTransformFn ::
  TemplatingEngine ->
  MethodTransformFn ->
  Validation TransformErrorBundle ()
validateMethodTransformFn _engine = \case
  Replace _method -> pure ()
