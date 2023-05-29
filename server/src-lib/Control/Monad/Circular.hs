{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Knot-tying monad transformer for recursive graph building.
--
-- Some operations, such as building a graph, are inherently self-recursive;
-- consider the following graph:
--
--    > a -> b
--    > b -> a
--
-- To construct in Haskell, we might want to use the following type:
--
--    > data Node = Node
--    >   { nodeName :: Text
--    >   , nodeNeighbours :: [Node]
--    >   }
--
-- To construct our trivial graph, we need @a@ to know about @b@ and @b@ to know
-- about @a@: this is fine as long as we can build them both at the same time:
--
--    > graph = [nodeA, nodeB]
--    >   where
--    >     nodeA = Node "a" [nodeB]
--    >     nodeB = Node "b" [nodeA]
--
-- But this falls apart as soon as building the nodes becomes more complicated;
-- for instance, if it becomes monadic. This causes an infinite recursion:
--
--    > graph = do
--    >   a <- buildA
--    >   b <- buildB
--    >   pure [a,b]
--    >   where
--    >     buildA = do
--    >       b <- buildB
--    >       pure $ Node "a" [b]
--    >     buildB = do
--    >       a <- buildA
--    >       pure $ Node "b" [a]
--
-- The reason why the non-monadic version works is laziness; and there is a way
-- to retrieve this laziness in a monadic context: it's what 'MonadFix' is for.
-- (https://wiki.haskell.org/MonadFix)
--
-- However, 'MonadFix' is both powerful and unintuitive; the goal of this module
-- is to use its power, but to give it a more restricted interface, to make it
-- easier to use. Using 'CircularT', the graph above can be built monadically
-- like so:
--
--    > graph = runCircularT do
--    >   a <- buildA
--    >   b <- buildB
--    >   pure [a,b]
--    >   where
--    >     buildA = withCircular "a" do
--    >       b <- buildB
--    >       pure $ Node "a" [b]
--    >     buildB = withCircular "b" do
--    >       a <- buildA
--    >       pure $ Node "b" [a]
--
-- It allows each part of a recursive process to be given a name (the type of
-- which is of the user's choosing), and it automatically breaks cycles. The
-- only caveat is that we cannot violate temporal causality: if we attempt to
-- make a cache-building decision based on the value obtained from the cache,
-- then no amount of laziness can save us:
--
--    > broken = runCircularT go
--    >   where
--    >     go = withCircular () do
--    >       x <- go
--    >       pure $ if odd x then 1 else 0
--
-- `CircularT` is somewhat similar to `TardisT` from @Control.Monad.Tardis@ and
-- `SchemaT` from @Hasura.GraphQL.Parser.Monad@, but simpler than both.
module Control.Monad.Circular
  ( CircularT,
    runCircularT,
    withCircular,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Prelude

-- | CircularT is implemented as a state monad containing a lazy HashMap.
--
-- We use this state to both determine wether we have already encountered a
-- given key and to track the associated result. We use laziness and MonadFix to
-- tie the knot for us (see 'withCircular').
--
-- - type @k@ is the type of cache key, to which a given action is associated.
-- - type @v@ is the values we wish to cache in our process.
-- - type @m@ is the underlying monad on which this transformer operates.
-- - type @a@ is the result of the computation
newtype CircularT k v m a = CircularT (StateT (HashMap k v) m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadReader r,
      MonadWriter w
    )

instance MonadTrans (CircularT k v) where
  lift = CircularT . lift

-- | Allow code in 'CircularT' to have access to any underlying state
-- capabilities, hiding the fact that 'CircularT' itself is a state monad.
instance (MonadState s m) => MonadState s (CircularT k v m) where
  get = lift get
  put x = lift $ put x

-- | Runs a computation in 'CircularT'.
runCircularT :: (Hashable k, MonadFix m) => CircularT k v m a -> m a
runCircularT (CircularT m) = evalStateT m mempty

-- | Cache a computation under a given key.
--
-- For a given key @k@, and a computation in 'CircularT' that yields a value of
-- type @v@, return an action that builds said value @v@ but that prevents
-- cycles by looking into and populating a stateful cache.
withCircular ::
  (Hashable k, MonadFix m) =>
  k ->
  CircularT k v m v ->
  CircularT k v m v
withCircular key (CircularT action) = CircularT do
  cache <- get
  case Map.lookup key cache of
    -- If the key is already present in the cache, that means we have
    -- already encountered that particular key in our process; no need to use the
    -- @action@.
    Just value -> pure value
    -- Otherwise, it means we haven't encountered it yet: we need to build it
    -- and cache the result.
    Nothing -> mdo
      -- Insert a thunk referencing the eventual actual value in the cache; we
      -- need the cache to be a lazy map for this to work.
      modify $ Map.insert key actualValue
      -- We compute the actual value by evaluating the action. This will only
      -- happen once per key. Note that we use 'actualValue' before it is built:
      -- this is why we need 'MonadFix' and "recursive do".
      actualValue <- action
      -- And we return the value!
      pure actualValue

-- We don't want to rely on Hasura.Prelude in "third-party" libraries.
{-# ANN withCircular ("HLint: ignore Use onNothing" :: String) #-}
