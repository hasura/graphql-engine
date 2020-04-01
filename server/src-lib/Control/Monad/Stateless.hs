{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Stateless
  ( MonadStateless(..)
  ) where

import           Prelude

import           Control.Monad.Trans.Control
import           Data.Type.Equality

class (MonadBaseControl b m, forall a. StatelessProof m a) => MonadStateless b m | m -> b where
  liftWithStateless :: ((forall a. m a -> b a) -> b c) -> m c

instance (MonadBaseControl b m, forall a. StatelessProof m a) => MonadStateless b m where
  liftWithStateless f = liftBaseWith $ \lowerBase ->
    let lowerBasePure :: forall a. m a -> b a
        lowerBasePure m =  case statelessProof @m @a of
          Refl -> lowerBase m
    in f lowerBasePure

class (StM m a ~ a) => StatelessProof m a where
  statelessProof :: StM m a :~: a
instance (StM m a ~ a) => StatelessProof m a where
  statelessProof = Refl
