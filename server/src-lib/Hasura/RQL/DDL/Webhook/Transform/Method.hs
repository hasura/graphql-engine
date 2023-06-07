{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.RQL.DDL.Webhook.Transform.Method
  ( -- * Method transformations
    Method (..),
    TransformFn (..),
    TransformCtx (..),
    MethodTransformFn (..),
  )
where

-------------------------------------------------------------------------------

import Data.Validation
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request (RequestTransformCtx)
import Hasura.RQL.Types.Webhook.Transform.Method (Method (..), MethodTransformFn (..), TransformCtx (..), TransformFn (..))

-------------------------------------------------------------------------------

instance Transform Method where
  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyMethodTransformFn' is defined
  -- separately.
  transform (MethodTransformFn_ fn) (TransformCtx reqCtx) = applyMethodTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateMethodTransformFn' is defined
  -- separately.
  validate engine (MethodTransformFn_ fn) = validateMethodTransformFn engine fn

-- | Provide an implementation for the transformations defined by
-- 'MethodTransformFn'.
--
-- If one views 'MethodTransformFn' as an interface describing HTTP method
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyMethodTransformFn ::
  (MonadError TransformErrorBundle m) =>
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
