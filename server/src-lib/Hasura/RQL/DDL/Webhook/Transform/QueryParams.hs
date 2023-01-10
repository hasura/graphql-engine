{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.Webhook.Transform.QueryParams
  ( -- * Query transformations
    QueryParams (..),
    TransformFn (..),
    TransformCtx (..),
    QueryParamsTransformFn (..),
  )
where

-------------------------------------------------------------------------------

import Autodocodec (HasCodec (codec), dimapCodec, disjointEitherCodec, hashMapCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as M
import Data.Validation (Validation)
import Data.Validation qualified as V
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( TemplatingEngine,
    Transform (..),
    TransformErrorBundle (..),
    UnescapedTemplate (..),
  )
import Hasura.RQL.DDL.Webhook.Transform.Request
  ( RequestTransformCtx,
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform',
  )
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.HTTP.Types.URI (parseQuery)

-------------------------------------------------------------------------------

-- | The actual query params we are transforming.
--
-- This newtype is necessary because otherwise we end up with an
-- orphan instance.
newtype QueryParams = QueryParams {unQueryParams :: HTTP.Query}

instance Transform QueryParams where
  -- NOTE: GHC does not let us attach Haddock documentation to data family
  -- instances, so 'QueryParamsTransformFn' is defined separately from this
  -- wrapper.
  newtype TransformFn QueryParams
    = QueryParamsTransformFn_ QueryParamsTransformFn
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, FromJSON, ToJSON)

  newtype TransformCtx QueryParams = TransformCtx RequestTransformCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'applyQueryParamsTransformFn' is defined
  -- separately.
  transform (QueryParamsTransformFn_ fn) (TransformCtx reqCtx) = applyQueryParamsTransformFn fn reqCtx

  -- NOTE: GHC does not let us attach Haddock documentation to typeclass
  -- method implementations, so 'validateQueryParamsTransformFn' is defined
  -- separately.
  validate engine (QueryParamsTransformFn_ fn) =
    validateQueryParamsTransformFn engine fn

-- | The defunctionalized transformation 'QueryParams'
data QueryParamsTransformFn
  = AddOrReplace [(UnescapedTemplate, Maybe UnescapedTemplate)]
  | ParamTemplate UnescapedTemplate
  deriving (NFData)
  deriving stock (Eq, Generic, Show)

-- | Provide an implementation for the transformations defined by
-- 'QueryParamsTransformFn'.
--
-- If one views 'QueryParamsTransformFn' as an interface describing HTTP method
-- transformations, this can be seen as an implementation of these
-- transformations as normal Haskell functions.
applyQueryParamsTransformFn ::
  MonadError TransformErrorBundle m =>
  QueryParamsTransformFn ->
  RequestTransformCtx ->
  QueryParams ->
  m QueryParams
applyQueryParamsTransformFn fn context _oldQueryParams = case fn of
  AddOrReplace addOrReplaceParams -> do
    -- NOTE: We use `ApplicativeDo` here to take advantage of Validation's
    -- applicative sequencing
    queryParams <- liftEither . V.toEither $
      for addOrReplaceParams \(rawKey, rawValue) -> do
        key <- runUnescapedRequestTemplateTransform' context rawKey
        value <- traverse (runUnescapedRequestTemplateTransform' context) rawValue
        pure $
          if key == "null" || value == Just "null"
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

instance HasCodec QueryParamsTransformFn where
  codec = dimapCodec dec enc $ disjointEitherCodec addOrReplaceCodec templateCodec
    where
      addOrReplaceCodec = hashMapCodec (codec @(Maybe UnescapedTemplate))
      templateCodec = codec @UnescapedTemplate

      dec (Left qps) = AddOrReplace $ M.toList qps
      dec (Right template) = ParamTemplate template

      enc (AddOrReplace addOrReplace) = Left $ M.fromList addOrReplace
      enc (ParamTemplate template) = Right template

instance J.ToJSON QueryParamsTransformFn where
  toJSON (AddOrReplace addOrReplace) = J.toJSON $ M.fromList addOrReplace
  toJSON (ParamTemplate template) = J.toJSON template

instance J.FromJSON QueryParamsTransformFn where
  parseJSON xs@(J.Object _) = AddOrReplace . M.toList <$> J.parseJSON xs
  parseJSON xs@(J.String _) = ParamTemplate <$> J.parseJSON xs
  parseJSON _ = fail "Invalid query parameter"
