{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.RQL.DDL.Webhook.Transform.QueryParams
  ( -- * Query transformations
    QueryParams (..),
    TransformFn (..),
    TransformCtx (..),
    QueryParamsTransformFn (..),
  )
where

-------------------------------------------------------------------------------

import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Hasura.RQL.Types.Webhook.Transform.QueryParams (QueryParams (..), QueryParamsTransformFn (..), TransformCtx (..), TransformFn (..))
import Network.HTTP.Types.URI (parseQuery)

-------------------------------------------------------------------------------

instance Transform QueryParams where
  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyQueryParamsTransformFn' is defined
  -- separately.
  transform (QueryParamsTransformFn_ fn) (TransformCtx reqCtx) = applyQueryParamsTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateQueryParamsTransformFn' is defined
  -- separately.
  validate engine (QueryParamsTransformFn_ fn) =
    validateQueryParamsTransformFn engine fn

-- | Provide an implementation for the transformations defined by
-- 'QueryParamsTransformFn'.
--
-- If one views 'QueryParamsTransformFn' as an interface describing HTTP method
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyQueryParamsTransformFn ::
  (MonadError TransformErrorBundle m) =>
  QueryParamsTransformFn ->
  RequestTransformCtx ->
  QueryParams ->
  m QueryParams
applyQueryParamsTransformFn fn context _oldQueryParams = case fn of
  AddOrReplace addOrReplaceParams -> do
    -- NOTE: We use `ApplicativeDo` here to take advantage of Validation's
    -- applicative sequencing
    queryParams <- liftEither
      . V.toEither
      $ for addOrReplaceParams \(rawKey, rawValue) -> do
        key <- runUnescapedRequestTemplateTransform' context rawKey
        value <- traverse (runUnescapedRequestTemplateTransform' context) rawValue
        pure
          $ if key == "null" || value == Just "null"
            then Nothing
            else Just (key, value)
    pure $ QueryParams (catMaybes queryParams)
  ParamTemplate template -> do
    resolvedValue <- liftEither . V.toEither $ runUnescapedRequestTemplateTransform' context template
    pure $ QueryParams (parseQuery resolvedValue)

-- | Validate that the provided 'QueryParamsTransformFn' is correct in the
-- context of a particular 'TemplatingEngine'.
--
-- This is a product of the fact that the correctness of a given transformation
-- may be dependent on zero, one, or more of the templated transformations
-- encoded within the given 'QueryParamsTransformFn'.
validateQueryParamsTransformFn ::
  TemplatingEngine ->
  QueryParamsTransformFn ->
  Validation TransformErrorBundle ()
validateQueryParamsTransformFn engine = \case
  AddOrReplace addOrReplaceParams ->
    -- NOTE: We use `ApplicativeDo` here to take advantage of
    -- Validation's applicative sequencing
    for_ addOrReplaceParams \(key, val) -> do
      validateRequestUnescapedTemplateTransform' engine key
      traverse_ (validateRequestUnescapedTemplateTransform' engine) val
      -- NOTE: There's a bug in `ApplicativeDo` which infers a `Monad`
      -- constraint on this block if it doens't end with `pure ()`
      pure ()
  ParamTemplate template -> do
    validateRequestUnescapedTemplateTransform' engine template
    pure ()
{-# ANN validateQueryParamsTransformFn ("HLint: ignore Redundant pure" :: String) #-}
