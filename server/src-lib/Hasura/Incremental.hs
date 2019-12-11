{-# LANGUAGE Arrows #-}

-- | A simple implementation of /incremental build rules/, which can be used to avoid unnecessary
-- recomputation on incrementally-changing input. See 'Rule' for more details.
module Hasura.Incremental
  ( Rule
  , Result
  , build
  , rebuild
  , rebuildRule
  , result

  , ArrowCache(..)
  , ArrowDistribute(..)
  ) where

import           Hasura.Prelude         hiding (id, (.))

import qualified Data.HashMap.Strict    as M

import           Control.Applicative
import           Control.Arrow.Extended
import           Control.Category
import           Data.Profunctor
import           Data.Tuple             (swap)

-- | A value of type @'Rule' m a b@ is a /build rule/: a computation that describes how to build a
-- value of type @b@ from a value of type @a@ in a monad @m@. What distinguishes @'Rule' m a b@ from
-- an ordinary function of type @a -> m b@ is that it can be made /incremental/ (in the sense of
-- “incremental compilation”) — after executing it, future executions can perform a subset of the
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
  -- Note: this is a CPS encoding of `a -> m (Result m a b)`. In practice, the CPS encoding seems to
  -- provide meaningful performance improvements: it cuts down significantly on allocation and is
  -- friendlier to GHC’s optimizer.
  = Rule (forall r. a -> (b -> Rule m a b -> m r) -> m r)

build :: (Applicative m) => Rule m a b -> a -> m (Result m a b)
build (Rule r) a = r a \b r' -> pure $ Result b r'
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
Rule f `rComp` Rule g = Rule \a k -> g a \b g' -> f b \c f' -> k c (f' `rComp` g')
{-# INLINABLE[0] rComp #-}
{-# RULES "associate" forall f g h. f `rComp` (g `rComp` h) = (f `rComp` g) `rComp` h #-}

rId :: Rule m a a
rId = Rule \a k -> k a rId
{-# INLINABLE[0] rId #-}
{-# RULES
"f/id" forall f. f `rComp` rId = f
"id/f" forall f. rId `rComp` f = f
#-}

rArr :: (a -> b) -> Rule m a b
rArr f = Rule \a k -> k (f a) (rArr f)
{-# INLINABLE[0] rArr #-}
{-# RULES
"arr/id"        rArr (\x -> x) = rId
"arr/const" [1] forall x. rArr (\_ -> x) = rPure x
"arr/arr"       forall f g. rArr f `rComp` rArr g = rArr (f . g)
"arr/arr/f"     forall f g h. (f `rComp` rArr g) `rComp` rArr h = f `rComp` rArr (g . h)
#-}

rArrM :: (Monad m) => (a -> m b) -> Rule m a b
rArrM f = Rule \a k -> f a >>= \b -> k b (rArrM f)
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
rFirst (Rule r) = Rule \(a, c) k -> r a \b r' -> k (b, c) (rFirst r')
{-# INLINABLE[0] rFirst #-}
{-# RULES
"first/id"         rFirst rId = rId
"first/arr"        forall f. rFirst (rArr f) = rArr (second f)
"first/arrM"       forall f. rFirst (rArrM f) = rArrM (runKleisli (first (Kleisli f)))
"first/push"  [~1] forall f g. rFirst (f `rComp` g) = rFirst f `rComp` rFirst g
"first/pull"   [1] forall f g. rFirst f `rComp` rFirst g = rFirst (f `rComp` g)
"first/f/pull" [1] forall f g h. (f `rComp` rFirst g) `rComp` rFirst h = f `rComp` rFirst (g `rComp` h)
#-}

rLeft :: Rule m a b1 -> Rule m (Either a b2) (Either b1 b2)
rLeft r0 = go r0 where
  go (Rule r) = Rule \e k -> case e of
    Left  a -> r a \b r' -> k (Left b) (go r')
    Right c -> k (Right c) (go r0)
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
rPure a = Rule \_ k -> k a (rPure a)
{-# INLINABLE[0] rPure #-}
{-# RULES "pure/push" [~1] rPure = rArr . const #-} -- see Note [Desugaring derived operations]

rSecond :: Rule m a1 b -> Rule m (a2, a1) (a2, b)
rSecond (Rule r) = Rule \(c, a) k -> r a \b r' -> k (c, b) (rSecond r')
{-# INLINABLE[0] rSecond #-}
-- see Note [Desugaring derived operations]
{-# RULES "second/push" [~1] forall f. rSecond f = rArr swap . rFirst f . rArr swap #-}

swapEither :: Either a b -> Either b a
swapEither = either Right Left
{-# INLINE[0] swapEither #-}

rRight :: Rule m a1 b -> Rule m (Either a2 a1) (Either a2 b)
rRight r0 = go r0 where
  go (Rule r) = Rule \e k -> case e of
    Left  c -> k (Left c) (go r0)
    Right a -> r a \b r' -> k (Right b) (go r')
{-# INLINABLE[0] rRight #-}
-- see Note [Desugaring derived operations]
{-# RULES "right/push" [~1] forall f. rRight f = rArr swapEither . rLeft f . rArr swapEither #-}

rSplit :: Rule m a1 b1 -> Rule m a2 b2 -> Rule m (a1, a2) (b1, b2)
Rule f `rSplit` Rule g = Rule \(a, b) k -> f a \c f' -> g b \d g' -> k (c, d) (f' `rSplit` g')
{-# INLINABLE[0] rSplit #-}
-- see Note [Desugaring derived operations]
{-# RULES "***/push" [~1] forall f g. f `rSplit` g = rSecond g . rFirst f #-}

rFanout :: Rule m a b1 -> Rule m a b2 -> Rule m a (b1, b2)
Rule f `rFanout` Rule g = Rule \a k -> f a \b f' -> g a \c g' -> k (b, c) (f' `rFanout` g')
{-# INLINABLE[0] rFanout #-}
-- see Note [Desugaring derived operations]
{-# RULES "&&&/push" [~1] forall f g. f `rFanout` g = (f *** g) . rArr (\a -> (a, a)) #-}

rFork :: Rule m a1 b1 -> Rule m a2 b2 -> Rule m (Either a1 a2) (Either b1 b2)
f0 `rFork` g0 = go f0 g0 where
  go (Rule f) (Rule g) = Rule \e k -> case e of
    Left  a -> f a \b f' -> k (Left  b) (go f' g0)
    Right a -> g a \b g' -> k (Right b) (go f0 g')
{-# INLINABLE[0] rFork #-}
-- see Note [Desugaring derived operations]
{-# RULES "+++/push" [~1] forall f g. f `rFork` g = rRight g . rLeft f #-}

fromEither :: Either a a -> a
fromEither = either id id
{-# INLINE[0] fromEither #-}

rFanin :: Rule m a1 b -> Rule m a2 b -> Rule m (Either a1 a2) b
f0 `rFanin` g0 = go f0 g0 where
  go (Rule f) (Rule g) = Rule \e k -> case e of
    Left  a -> f a \b f' -> k b (go f' g0)
    Right a -> g a \b g' -> k b (go f0 g')
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

class (Arrow arr) => ArrowCache arr where
  -- | Adds equality-based caching to the given rule. After each execution of the rule, its input
  -- and result values are cached. On the next rebuild, the input value is compared via '==' to the
  -- previous input value. If they are the same, the previous build result is returned /without/
  -- re-executing the rule. Otherwise, the old cached values are discarded, and the rule is
  -- re-executed to produce a new set of cached values.
  --
  -- Indescriminate use of 'cache' is likely to have little effect except to increase memory usage,
  -- since the input and result of each rule execution must be retained in memory. Avoid using
  -- 'cache' around rules with large input or output that is likely to change often unless profiling
  -- indicates it is computationally expensive enough to be worth the memory overhead.
  --
  -- __Note that only direct inputs and outputs of a 'Rule' are cached.__ It is extremely important
  -- to take care in your choice of the base monad @m@:
  --
  --   * Monads that provide access to extra information through a side-channel, such as 'ReaderT',
  --     'StateT', or 'IO', will __not__ expose that information to dependency analysis. If that
  --     information changes between builds, but the rule’s direct inputs remain unchanged, the rule
  --     will __not__ be re-executed.
  --
  --   * Dually, monads that perform side-effects as part of execution, such as 'StateT', 'WriterT',
  --     or 'IO', will __not__ have their side-effects automatically replayed if the cached result
  --     is used. If the side effects are only necessary to change some state to bring it in line
  --     with the updated inputs, that is entirely fine (and likely even desirable), but if the
  --     side-effects are necessary to produce each result, caching will lead to incorrect behavior.
  --
  -- The safest monad to use for @m@ is therefore 'Identity', which suffers neither of the above
  -- problems by construction. However, in practice, it is highly desirable to be able to execute
  -- rules that may perform side-effects in 'IO', so the capability is exposed.
  --
  -- For a safe way to use other effects with 'Rule', use arrow transformers like 'ErrorA',
  -- 'ReaderA', and 'WriterA' on top of a base @'Rule' m@ arrow. Such uses are completely safe, as
  -- the extra information added by other transformers /will/ be exposed to dependency analysis and
  -- /will/ be cached.
  cache :: (Eq a) => arr a b -> arr a b

instance (ArrowChoice arr, ArrowCache arr) => ArrowCache (ErrorA e arr) where
  cache (ErrorA f) = ErrorA (cache f)
  {-# INLINE cache #-}
instance (Monoid w, ArrowCache arr) => ArrowCache (WriterA w arr) where
  cache (WriterA f) = WriterA (cache f)
  {-# INLINE cache #-}

instance ArrowCache (Rule m) where
  cache (Rule r0) = Rule \a k -> r0 a \b r0' -> k b (cached a b r0')
    where
      cached a b (Rule r) = Rule \a' k -> if
        | a == a'   -> k b (cached a b (Rule r))
        | otherwise -> r a' \b' r' -> k b' (cached a' b' r')
  {-# INLINABLE cache #-}

class (Arrow arr) => ArrowDistribute arr where
  -- | Distributes an arrow that operates on key-value pairs, over a 'M.HashMap' in an
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
  keyed r0 = keyedWith M.empty
    where
      keyedWith
        :: HashMap k (Rule m (e, (k, (a, s))) b)
        -> Rule m (e, (HashMap k a, s)) (HashMap k b)
      keyedWith !rs = Rule \(e, (vs, s)) c ->
        M.foldrWithKey (process rs e s) (finish c) vs M.empty M.empty

      process
        :: HashMap k (Rule m (e, (k, (a, s))) b)
        -> e -> s -> k -> a
        -> (HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r)
        -> HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r
      process rs e s k a c !vs' !rs' =
        let Rule r = M.lookupDefault r0 k rs
        in r (e, (k, (a, s))) \b r' -> c (M.insert k b vs') (M.insert k r' rs')

      finish
        :: (HashMap k b -> Rule m (e, (HashMap k a, s)) (HashMap k b) -> m r)
        -> HashMap k b -> HashMap k (Rule m (e, (k, (a, s))) b) -> m r
      finish c !vs' !rs' = c vs' (keyedWith rs')
  {-# INLINABLE keyed #-}
