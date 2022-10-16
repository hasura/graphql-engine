-- | The 'WithOptional' Functor and associated operations.
module Hasura.RQL.DDL.Webhook.Transform.WithOptional
  ( WithOptional (..),
    withOptional,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (Coercible)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

-------------------------------------------------------------------------------

-- | Enrich a 'Functor' @f@ with optionality; this is primarily useful when
-- one wants to annotate fields as optional when using the Higher-Kinded Data
-- pattern.
--
-- 'WithOptional'@ f@ is equivalent to @Compose Maybe f@.
newtype WithOptional f result = WithOptional
  { getOptional :: Maybe (f result)
  }
  deriving stock (Eq, Functor, Foldable, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

deriving newtype instance
  (Cacheable (f result)) =>
  Cacheable (WithOptional f result)

deriving newtype instance
  (NFData (f result)) =>
  NFData (WithOptional f result)

-- | 'WithOptional' smart constructor for the special case of optional values
-- that are representationally equivalent to some "wrapper" type.
--
-- For example:
-- @
-- withOptional \@HeaderTransformsAction headers == WithOptional $ fmap HeadersTransform headers
-- @
--
-- In other words: this function observes the isomorphism between @'Maybe' a@
-- and  @'WithOptional' f b@ if an isomorphism exists between @a@ and @f b@.
withOptional ::
  forall a b f.
  Coercible a (f b) =>
  Maybe a ->
  WithOptional f b
withOptional = coerce
