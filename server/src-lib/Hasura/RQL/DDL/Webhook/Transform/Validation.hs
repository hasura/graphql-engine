{-# LANGUAGE PolyKinds #-}

-- | We validate 'TransformFn' terms inside 'RequestTransform' before
-- dispatching Metadata actions in 'runMetadataQueryV1M'. Validation
-- follows the same HKD pattern from 'applyRequestTransform' but using
-- 'btraverseC' to call 'validate' from the 'Transform' class on all
-- the HKD fields.
module Hasura.RQL.DDL.Webhook.Transform.Validation
  ( Unvalidated (..),
    Unvalidated1 (..),
    unUnvalidate,
    unUnvalidate1,
    validateRequestTransform,
    validateTransforms,
  )
where

import Control.Lens (Lens', LensLike, lens, traverseOf)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Functor.Barbie (FunctorB (bmap), btraverseC)
import Data.Functor.Compose (Compose (..))
import Data.Kind
import Data.Validation (Validation, toEither)
import Hasura.Base.Error (QErr)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.DDL.Webhook.Transform.Class

-------------------------------------------------------------------------------

type Tuple1 a b = Compose ((,) a) b

type OptionalTuple1 a b = WithOptional (Tuple1 a b)

-- | A variation on 'RequestTransformFn' where 'TransformFn' is tupled
-- with 'TemplatingEngine'. This is necessary to validate the 'TransformFn'.
--
-- TODO: In the future we most likely want to embed the
-- 'TemplatingEngine' in the 'TransformFn' or the
-- 'Template'/'UnwrappedTemplate', in which case we would not need
-- this alias for validation.
type ValidationFields = RequestFields (OptionalTuple1 TemplatingEngine TransformFn)

-- TODO(SOLOMON): Add lens law unit tests

-- | A lens for zipping our defunctionalized transform with the
-- 'TemplatingEngine' for validation.
transformFns :: Lens' RequestTransform ValidationFields
transformFns = lens getter setter
  where
    getter :: RequestTransform -> ValidationFields
    getter RequestTransform {..} =
      bmap (WithOptional . fmap (Compose . (templateEngine,)) . getOptional) requestFields

    setter :: RequestTransform -> ValidationFields -> RequestTransform
    setter rt requestFields' =
      rt {requestFields = bmap (WithOptional . fmap (snd . getCompose) . getOptional) requestFields'}

-- | Validate all 'TransformFn a' fields in the 'RequestTransform'.
validateRequestTransform ::
  MonadError TransformErrorBundle m =>
  RequestTransform ->
  m RequestTransform
validateRequestTransform reqTransform =
  liftEither $ toEither $ transformFns (btraverseC @Transform validate') reqTransform
  where
    validate' ::
      (Transform a) =>
      OptionalTuple1 TemplatingEngine TransformFn a ->
      Validation TransformErrorBundle (OptionalTuple1 TemplatingEngine TransformFn a)
    validate' = \case
      fn@(WithOptional (Just (Compose (engine, transformFn)))) ->
        fn <$ validate engine transformFn
      fn -> pure fn

-------------------------------------------------------------------------------

-- | Used to annotate that a 'RequestTransform', or some record
-- containing a 'RequestTransform' has not yet been validated.
newtype Unvalidated a = Unvalidated {_unUnvalidate :: a}
  deriving newtype (FromJSON, ToJSON)

-- | A lens for focusing through 'Unvalidated' in 'validateTransforms'.
unUnvalidate :: Lens' (Unvalidated a) a
unUnvalidate = lens _unUnvalidate (\_ a -> Unvalidated a)

-- | Used to annotate that a higher kinded type containing a
-- 'RequestTransform' has not yet been validated.
--
-- This is needed specifically for 'CreateEventTriggerQuery' and any
-- other type that is paramterized by a 'BackendType'.
newtype Unvalidated1 (f :: k -> Type) (a :: k) = Unvalidated1 {_unUnvalidate1 :: f a}
  deriving newtype (FromJSON, ToJSON)

-- | A lens for focusing through 'Unvalidated1' in 'validateTransforms'.
unUnvalidate1 :: Lens' (Unvalidated1 f a) (f a)
unUnvalidate1 = lens _unUnvalidate1 (\_ a -> Unvalidated1 a)

-- | Used to focus into a records in 'RQLMetadataV1' and validate any
-- 'RequestTransform' terms present.
validateTransforms ::
  (MonadError QErr m) =>
  LensLike (Either TransformErrorBundle) api api RequestTransform RequestTransform ->
  (api -> m EncJSON) ->
  api ->
  m EncJSON
validateTransforms focus f q =
  case traverseOf focus validateRequestTransform q of
    Left error' -> pure $ encJFromJValue $ J.toJSON error'
    Right q' -> f q'
