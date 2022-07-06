module Hasura.GraphQL.Parser.Names
  ( MkTypename (..),
    mkTypename,
    withTypenameCustomization,
    HasName (..),
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader, asks, local)
import Data.Has
import Data.Monoid
import Language.GraphQL.Draft.Syntax (Name (..))
import Prelude

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName = id

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
