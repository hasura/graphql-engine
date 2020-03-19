{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines the basic 'Rule' datatype and its core operations.
module Hasura.Incremental.Internal.Rule where

import           Hasura.Prelude                         hiding (id, (.))

import qualified Data.HashMap.Strict                    as HM

import           Control.Applicative                    hiding (liftA)
import           Control.Arrow.Extended
import           Control.Category
import           Data.Profunctor
import           Data.Tuple                             (swap)

import           Hasura.Incremental.Internal.Dependency

-- | A value of type @'Rule' m a b@ is a /build rule/: a computation that describes how to build a
-- value of type @b@ from a value of type @a@ in a monad @m@. What distinguishes @'Rule' m a b@ from
-- an ordinary function of type @a -> m b@ is that it can be made /incremental/ (in the sense of
-- “incremental compilation”)—after executing it, future executions can perform a subset of the
-- required work if only a portion of the input changed.
--
-- To achieve this, 'Rule's have a more restrictive interface: there is no @Monad ('Rule' m a)@
-- instance, for example. Instead, 'Rule's are composed using the 'Arrow' hierarchy of operations,
-- which ensures that the dependency graph of build rules is mostly static (though it may contain
-- conditional branches, and combinators such as 'keyed' can express restricted forms of dynamic
-- dependencies). Each atomic rule may be defined using the 'Monad' instance for @m@, but
-- incrementalization is not supported inside those rules — they are treated as a single, monolithic
-- computation.
--
-- Atomic rules are created with the 'arrM' function, and caching can be added to a rule using the
-- 'cache' combinator. Rules can be executed using the 'build' function, which returns a 'Result'. A
-- 'Result' contains the built value, accessible via 'result', but it also allows supplying a new
-- input value using 'rebuild' to produce a new result incrementally.
newtype Rule m a b
  -- Note: this is a CPS encoding of `Accesses -> a -> m (Result m a b)`. In practice, the CPS
  -- encoding seems to provide meaningful performance improvements: it cuts down significantly on
  -- allocation and is friendlier to GHC’s optimizer.
  = Rule (forall r. Accesses -> a -> (Accesses -> b -> Rule m a b -> m r) -> m r)

build :: (Applicative m) => Rule m a b -> a -> m (Result m a b)
build (Rule r) a = r mempty a \_ b r' -> pure $ Result b r'
{-# INLINE build #-}

data Result m a b
  = Result
  { result      :: !b
  , rebuildRule :: !(Rule m a b)
  } deriving (Functor)

rebuild :: (Applicative m) => Result m a b -> a -> m (Result m a b)
rebuild = build . rebuildRule
{-# INLINE rebuild #-}

{- Note [Rule rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained by Note [Arrow rewrite rules] in Control.Arrow.Extended, it’s important to define
type-specific rewrite rules to get good performance with arrows when the concrete type is used. This
is especially important for `Rule`, since the recursive definitions of operations like `.` and `arr`
are very difficult for the optimizer to deal with, and the composition of lots of small rules
created with `arr` is very inefficient.

Since GHC aggressively specializes and inlines class methods, the rules cannot be defined on the
class methods themselves. Instead, the class methods expand to auxiliary definitions, and those
definitions include an INLINABLE[0] pragma that ensures they do not inline until the final
optimization phase. The rules are defined in terms of those definitions, so they will be able to do
their work in prior phases.

Note [Desugaring derived operations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One subtlety to the above is that we want to define operations in terms of other operations as much
as possible to avoid the need to write an enormous number of rewrite rules, but if we define them
that way directly, then we’ll end up using needlessly inefficient implementations when the
operations aren’t specialized. Therefore, we provide efficient implementations of operations like
`second`, but aggressively rewrite them in terms of simpler primitives like `first` when GHC is able
to specialize them. -}

rComp :: Rule m a1 b -> Rule m a2 a1 -> Rule m a2 b
Rule f `rComp` Rule g = Rule \s a k -> g s a \s' b g' -> f s' b \s'' c f' -> k s'' c (f' `rComp` g')
{-# INLINABLE[0] rComp #-}
{-# RULES "associate" forall f g h. f `rComp` (g `rComp` h) = (f `rComp` g) `rComp` h #-}

rId :: Rule m a a
rId = Rule \s a k -> k s a rId
{-# INLINABLE[0] rId #-}
{-# RULES
"f/id" forall f. f `rComp` rId = f
"id/f" forall f. rId `rComp` f = f
#-}

rArr :: (a -> b) -> Rule m a b
rArr f = Rule \s a k -> k s (f a) (rArr f)
{-# INLINABLE[0] rArr #-}
{-# RULES
"arr/id"        rArr (\x -> x) = rId
"arr/const" [1] forall x. rArr (\_ -> x) = rPure x
"arr/arr"       forall f g. rArr f `rComp` rArr g = rArr (f . g)
"arr/arr/f"     forall f g h. (f `rComp` rArr g) `rComp` rArr h = f `rComp` rArr (g . h)
#-}

rArrM :: (Monad m) => (a -> m b) -> Rule m a b
rArrM f = Rule \s a k -> f a >>= \b -> k s b (rArrM f)
{-# INLINABLE[0] rArrM #-}
{-# RULES
"arrM/arrM"   forall f g. rArrM f `rComp` rArrM g = rArrM (f <=< g)
"arr/arrM"    forall f g. rArr f `rComp` rArrM g = rArrM (fmap f . g)
"arrM/arr"    forall f g. rArrM f `rComp` rArr g = rArrM (f . g)
"arrM/arrM/f" forall f g h. (f `rComp` rArrM g) `rComp` rArrM h = f `rComp` rArrM (g <=< h)
"arr/arrM/f"  forall f g h. (f `rComp` rArr g) `rComp` rArrM h = f `rComp` rArrM (fmap g . h)
"arrM/arr/f"  forall f g h. (f `rComp` rArrM g) `rComp` rArr h = f `rComp` rArrM (g . h)
#-}

rFirst :: Rule m a b1 -> Rule m (a, b2) (b1, b2)
rFirst (Rule r) = Rule \s (a, c) k -> r s a \s' b r' -> k s' (b, c) (rFirst r')
{-# INLINABLE[0] rFirst #-}
{-# RULES
"first/id"         rFirst rId = rId
"first/arr"        forall f. rFirst (rArr f) = rArr (first f)
"first/arrM"       forall f. rFirst (rArrM f) = rArrM (runKleisli (first (Kleisli f)))
"first/push"  [~1] forall f g. rFirst (f `rComp` g) = rFirst f `rComp` rFirst g
"first/pull"   [1] forall f g. rFirst f `rComp` rFirst g = rFirst (f `rComp` g)
"first/f/pull" [1] forall f g h. (f `rComp` rFirst g) `rComp` rFirst h = f `rComp` rFirst (g `rComp` h)
#-}

rLeft :: Rule m a b1 -> Rule m (Either a b2) (Either b1 b2)
rLeft r0 = go r0 where
  go (Rule r) = Rule \s e k -> case e of
    Left  a -> r s a \s' b r' -> k s' (Left b) (go r')
    Right c -> k s (Right c) (go r0)
{-# INLINABLE[0] rLeft #-}
{-# RULES
"left/id"         rLeft rId = rId
"left/arr"        forall f. rLeft (rArr f) = rArr (left f)
"left/arrM"       forall f. rLeft (rArrM f) = rArrM (runKleisli (left (Kleisli f)))
"left/push"  [~1] forall f g. rLeft (f `rComp` g) = rLeft f `rComp` rLeft g
"left/pull"   [1] forall f g. rLeft f `rComp` rLeft g = rLeft (f `rComp` g)
"left/f/pull" [1] forall f g h. (f `rComp` rLeft g) `rComp` rLeft h = f `rComp` rLeft (g `rComp` h)
#-}

rPure :: b -> Rule m a b
rPure a = Rule \s _ k -> k s a (rPure a)
{-# INLINABLE[0] rPure #-}
{-# RULES "pure/push" [~1] rPure = rArr . const #-} -- see Note [Desugaring derived operations]

rSecond :: Rule m a1 b -> Rule m (a2, a1) (a2, b)
rSecond (Rule r) = Rule \s (c, a) k -> r s a \s' b r' -> k s' (c, b) (rSecond r')
{-# INLINABLE[0] rSecond #-}
-- see Note [Desugaring derived operations]
{-# RULES "second/push" [~1] forall f. rSecond f = rArr swap . rFirst f . rArr swap #-}

swapEither :: Either a b -> Either b a
swapEither = either Right Left
{-# INLINE[0] swapEither #-}

rRight :: Rule m a1 b -> Rule m (Either a2 a1) (Either a2 b)
rRight r0 = go r0 where
  go (Rule r) = Rule \s e k -> case e of
    Left  c -> k s (Left c) (go r0)
    Right a -> r s a \s' b r' -> k s' (Right b) (go r')
{-# INLINABLE[0] rRight #-}
-- see Note [Desugaring derived operations]
{-# RULES "right/push" [~1] forall f. rRight f = rArr swapEither . rLeft f . rArr swapEither #-}

rSplit :: Rule m a1 b1 -> Rule m a2 b2 -> Rule m (a1, a2) (b1, b2)
Rule f `rSplit` Rule g =
  Rule \s (a, b) k -> f s a \s' c f' -> g s' b \s'' d g' -> k s'' (c, d) (f' `rSplit` g')
{-# INLINABLE[0] rSplit #-}
-- see Note [Desugaring derived operations]
{-# RULES "***/push" [~1] forall f g. f `rSplit` g = rSecond g . rFirst f #-}

rFanout :: Rule m a b1 -> Rule m a b2 -> Rule m a (b1, b2)
Rule f `rFanout` Rule g =
  Rule \s a k -> f s a \s' b f' -> g s' a \s'' c g' -> k s'' (b, c) (f' `rFanout` g')
{-# INLINABLE[0] rFanout #-}
-- see Note [Desugaring derived operations]
{-# RULES "&&&/push" [~1] forall f g. f `rFanout` g = (f *** g) . rArr (\a -> (a, a)) #-}

rFork :: Rule m a1 b1 -> Rule m a2 b2 -> Rule m (Either a1 a2) (Either b1 b2)
f0 `rFork` g0 = go f0 g0 where
  go (Rule f) (Rule g) = Rule \s e k -> case e of
    Left  a -> f s a \s' b f' -> k s' (Left  b) (go f' g0)
    Right a -> g s a \s' b g' -> k s' (Right b) (go f0 g')
{-# INLINABLE[0] rFork #-}
-- see Note [Desugaring derived operations]
{-# RULES "+++/push" [~1] forall f g. f `rFork` g = rRight g . rLeft f #-}

fromEither :: Either a a -> a
fromEither = either id id
{-# INLINE[0] fromEither #-}

rFanin :: Rule m a1 b -> Rule m a2 b -> Rule m (Either a1 a2) b
f0 `rFanin` g0 = go f0 g0 where
  go (Rule f) (Rule g) = Rule \s e k -> case e of
    Left  a -> f s a \s' b f' -> k s' b (go f' g0)
    Right a -> g s a \s' b g' -> k s' b (go f0 g')
{-# INLINABLE[0] rFanin #-}
-- see Note [Desugaring derived operations]
{-# RULES "|||/push" [~1] forall f g. f `rFanin` g = rArr fromEither . (f +++ g) #-}

instance Functor (Rule m a) where
  fmap f r = arr f . r
  {-# INLINE fmap #-}
instance Applicative (Rule m a) where
  pure = rPure
  {-# INLINE pure #-}
  (<*>) = liftA2 ($)
  {-# INLINE (<*>) #-}
  liftA2 f g h = arr (uncurry f) . (g &&& h)
  {-# INLINE liftA2 #-}
instance Profunctor (Rule m) where
  dimap f g r = arr g . r . arr f
  {-# INLINE dimap #-}
  lmap f r = r . arr f
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}
instance Strong (Rule m) where
  first' = rFirst
  {-# INLINE first' #-}
  second' = rSecond
  {-# INLINE second' #-}
instance Choice (Rule m) where
  left' = rLeft
  {-# INLINE left' #-}
  right' = rRight
  {-# INLINE right' #-}
instance Category (Rule m) where
  id = rId
  {-# INLINE id #-}
  (.) = rComp
  {-# INLINE (.) #-}
instance Arrow (Rule m) where
  arr = rArr
  {-# INLINE arr #-}
  first = rFirst
  {-# INLINE first #-}
  second = rSecond
  {-# INLINE second #-}
  (***) = rSplit
  {-# INLINE (***) #-}
  (&&&) = rFanout
  {-# INLINE (&&&) #-}
instance ArrowChoice (Rule m) where
  left = rLeft
  {-# INLINE left #-}
  right = rRight
  {-# INLINE right #-}
  (+++) = rFork
  {-# INLINE (+++) #-}
  (|||) = rFanin
  {-# INLINE (|||) #-}
instance (Monad m) => ArrowKleisli m (Rule m) where
  arrM = rArrM
  {-# INLINE arrM #-}

class (Arrow arr) => ArrowDistribute arr where
  -- | Distributes an arrow that operates on key-value pairs, over a 'HM.HashMap' in an
  -- order-independent way.
  --
  -- This is intended to be used as a control operator in @proc@ notation; see
  -- Note [Weird control operator types] in "Control.Arrow.Extended".
  keyed
    :: (Eq k, Hashable k)
    => arr (e, (k, (a, s))) b
    -> arr (e, (HashMap k a, s)) (HashMap k b)

instance (Monoid w, ArrowDistribute arr) => ArrowDistribute (WriterA w arr) where
  keyed (WriterA f) = WriterA (arr (swap . sequence . fmap swap) . keyed f)
  {-# INLINE keyed #-}

-- | Unlike 'traverseA', using 'keyed' preserves incrementalization: if the input rule is
-- incremental in its argument, the resulting rule will be incremental as well for any entries in
-- the map that do not change between builds.
instance ArrowDistribute (Rule m) where
  keyed
    :: forall a b k e s
     . (Eq k, Hashable k)
    => Rule m (e, (k, (a, s))) b
    -> Rule m (e, (HashMap k a, s)) (HashMap k b)
  keyed r0 = keyedWith HM.empty
    where
      keyedWith
        :: HashMap k (Rule m (e, (k, (a, s))) b)
        -> Rule m (e, (HashMap k a, s)) (HashMap k b)
      keyedWith !rs = Rule \s (e, (vs, sk)) c ->
        HM.foldrWithKey (process rs e sk) (finish c) vs s HM.empty HM.empty

      process
        :: HashMap k (Rule m (e, (k, (a, s))) b)
        -> e -> s -> k -> a
        -> (Accesses -> HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r)
        -> Accesses -> HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r
      process rs e sk k a c s !vs' !rs' =
        let Rule r = HM.lookupDefault r0 k rs
        in r s (e, (k, (a, sk))) \s' b r' -> c s' (HM.insert k b vs') (HM.insert k r' rs')

      finish
        :: (Accesses -> HashMap k b -> Rule m (e, (HashMap k a, s)) (HashMap k b) -> m r)
        -> Accesses -> HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r
      finish c s !vs' !rs' = c s vs' (keyedWith rs')
  {-# INLINABLE keyed #-}
