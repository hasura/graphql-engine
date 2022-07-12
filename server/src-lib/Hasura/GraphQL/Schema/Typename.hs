module Hasura.GraphQL.Schema.Typename
  ( MkTypename (..),
    withTypenameCustomization,
    mkTypename,
  )
where

import Control.Lens (set)
import Data.Has (Has (..))
import Data.Monoid (Endo (..))
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax (Name)

-- | Type name customization
newtype MkTypename = MkTypename {runMkTypename :: Name -> Name}
  deriving (Semigroup, Monoid) via (Endo Name)

-- | Inject a new @MkTypename@ customization function into the environment.
-- This can be used by schema-building code (with @MonadBuildSchema@ constraint) to ensure
-- the correct type name customizations are applied.
withTypenameCustomization :: forall m r a. (MonadReader r m, Has MkTypename r) => MkTypename -> m a -> m a
withTypenameCustomization = local . set hasLens

-- | Apply the type name customization function from the current environment.
mkTypename :: (MonadReader r m, Has MkTypename r) => Name -> m Name
mkTypename name =
  ($ name) . runMkTypename <$> asks getter
