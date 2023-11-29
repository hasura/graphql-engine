module Control.Monad.Trans.Extended
  ( TransT (..),
  )
where

import Control.Monad.Morph
import Data.Kind
import Prelude

-- | Utility newtype that can be used to derive type class instances just using
-- `MonadTrans`.
--
-- We often derive some `MonadBlaBla` instance for `ReaderT` by using `lift`
-- from `MonadTrans`.  Which is fine, but it gets laborious if you do the same
-- for `ExceptT`, `StateT` and `WriterT`, even though the method implementations
-- are exactly the same.  `TransT` allows you to write one `MonadTrans`-based
-- instance, which can then be used with `DerivingVia` to use that one
-- implementation for all monad transformers that use that same lifting
-- implementation.
newtype TransT t (m :: Type -> Type) a = TransT (t m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MFunctor, MMonad)
