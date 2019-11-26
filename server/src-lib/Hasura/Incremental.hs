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

  , mapRule
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
  = Rule { build :: a -> m (Result m a b) }
  deriving (Functor)

-- | Modifies a 'Rule' by applying a natural transformation.
mapRule :: (Functor n) => (forall r. m r -> n r) -> Rule m a b -> Rule n a b
mapRule f rule = Rule \input -> f (build rule input) <&> \result' ->
  result' { rebuild = build (mapRule f (Rule $ rebuild result')) }

instance (Applicative m) => Applicative (Rule m a) where
  pure a = Rule . const . pure $ pure a
  rule1 <*> rule2 = Rule $ \input -> liftA2 (<*>) (build rule1 input) (build rule2 input)

instance (Functor m) => Profunctor (Rule m) where
  dimap f g (Rule build) = Rule (fmap (dimap f g) . build . f)

ruleArrM :: (Functor m) => (a -> m b) -> Rule m a b
ruleArrM f = Rule $ fix \build -> fmap (Result build) . f
{-# INLINABLE[0] ruleArrM #-}

ruleCompose :: (Monad m) => Rule m b c -> Rule m a b -> Rule m a c
ruleCompose rule2 rule1 = Rule $ \input -> do
  result1 <- build rule1 input
  result2 <- build rule2 (result result1)
  pure $ Result
    { rebuild = build (Rule (rebuild result2) `ruleCompose` Rule (rebuild result1))
    , result = result result2
    }
{-# INLINABLE[0] ruleCompose #-}

ruleFirst :: (Functor m) => Rule m a b -> Rule m (a, c) (b, c)
ruleFirst (Rule build) = Rule $ \(a, b) -> resultFirst b <$> build a
  where
    resultFirst b Result { rebuild, result } = Result
      { rebuild = \(a, b') -> resultFirst b' <$> rebuild a
      , result = (result, b)
      }
{-# INLINABLE[0] ruleFirst #-}

-- This is significantly trickier to implement than 'first'! Here’s how to think about it: the first
-- time the rule executes, we know nothing about previous runs, so if we’re given 'Left', we have to
-- call the original rule we’re given. At that point, as long as we are still given 'Left' on every
-- rebuild, we can take advantage of whatever caching happened on the previous run, so we keep
-- recursively calling 'leftResult'.
--
-- However, as soon as we get 'Right', we have to bail out. We return the input we’re given, and we
-- forget about any previous executions of the rule completely. If we’re given 'Left' on a
-- subsequent rebuild, we start over from the original rule again.
ruleLeft :: (Applicative m) => Rule m a b -> Rule m (Either a c) (Either b c)
ruleLeft (Rule build) = Rule eitherResult
  where
    eitherResult = either (fmap leftResult . build) rightResult
    leftResult Result { rebuild, result } = Result
      { rebuild = either (fmap leftResult . rebuild) rightResult
      , result = Left result
      }
    rightResult input = pure Result
      { rebuild = eitherResult
      , result = Right input
      }
{-# INLINABLE[0] ruleLeft #-}

firstM :: (Functor m) => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, b) = (, b) <$> f a
{-# INLINABLE firstM #-}

leftM :: (Applicative m) => (a -> m b) -> Either a c -> m (Either b c)
leftM f = \case
  Left  a -> Left <$> f a
  Right b -> pure $ Right b
{-# INLINABLE leftM #-}

{-# RULES -- see Note [Rule rewrite rules]
"Rule/associate"   forall f g h. f `ruleCompose` (g `ruleCompose` h) = (f `ruleCompose` g) `ruleCompose` h
"Rule/arrM/arrM"   forall f g. ruleArrM f `ruleCompose` ruleArrM g = ruleArrM (f <=< g)
"Rule/arrM/arrM/R" forall f g h. ruleArrM f `ruleCompose` (ruleArrM g `ruleCompose` h) = ruleArrM (f <=< g) `ruleCompose` h
"Rule/arrM/arrM/L" forall f g h. (f `ruleCompose` ruleArrM g) `ruleCompose` ruleArrM h = f `ruleCompose` ruleArrM (g <=< h)
"Rule/first/arrM"  forall f. ruleFirst (ruleArrM f) = ruleArrM (firstM f)
"Rule/left/arrM"   forall f. ruleLeft (ruleArrM f) = ruleArrM (leftM f)

"Rule/first/push" [~1] forall f g. ruleFirst (f `ruleCompose` g) = ruleFirst f `ruleCompose` ruleFirst g
"Rule/left/push"  [~1] forall f g. ruleLeft (f `ruleCompose` g) = ruleLeft f `ruleCompose` ruleLeft g
"Rule/first/pull"  [1] forall f g. ruleFirst f `ruleCompose` ruleFirst g = ruleFirst (f `ruleCompose` g)
"Rule/left/pull"   [1] forall f g. ruleLeft f `ruleCompose` ruleLeft g = ruleLeft (f `ruleCompose` g)
#-}

instance (Functor m) => Strong (Rule m) where
  first' = ruleFirst
  {-# INLINE first' #-}

instance (Applicative m) => Choice (Rule m) where
  left' = ruleLeft
  {-# INLINE left' #-}

instance (Monad m) => Category (Rule m) where
  id = arrM pure
  {-# INLINE id #-}
  (.) = ruleCompose
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (Rule m) where
  arr f = arrM (pure . f)
  {-# INLINE arr #-}
  first = ruleFirst
  {-# INLINE first #-}
  second f = arr swap . first f . arr swap
  {-# INLINE second #-}
  f *** g = second g . first f
  {-# INLINE (***) #-}
  f &&& g = (f *** g) . arr (\x -> (x, x))
  {-# INLINE (&&&) #-}

instance (Monad m) => ArrowChoice (Rule m) where
  left = ruleLeft
  {-# INLINE left #-}
  right f = arr (either Right Left) . ruleLeft f . arr (either Right Left)
  {-# INLINE right #-}
  f +++ g = right g . left f
  {-# INLINE (+++) #-}
  f ||| g = arr (either id id) . (f +++ g)
  {-# INLINE (|||) #-}

instance (Monad m) => ArrowKleisli m (Rule m) where
  arrM = ruleArrM
  {-# INLINE arrM #-}

data Result m a b
  = Result
  { rebuild :: !(a -> m (Result m a b))
  , result  :: !b
  } deriving (Functor)

rebuildRule :: Result m a b -> Rule m a b
rebuildRule = Rule . rebuild

instance (Applicative m) => Applicative (Result m a) where
  pure a = fix $ \result -> Result
    { rebuild = const $ pure result
    , result = a
    }
  result1 <*> result2 = Result
    { rebuild = \input -> liftA2 (<*>) (rebuild result1 input) (rebuild result2 input)
    , result = result result1 $ result result2
    }

instance (Functor m) => Profunctor (Result m) where
  dimap f g Result { rebuild, result } = Result
    { rebuild = fmap (dimap f g) . rebuild . f
    , result = g result
    }

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
instance (Monoid w, ArrowCache arr) => ArrowCache (WriterA w arr) where
  cache (WriterA f) = WriterA (cache f)

instance (Monad m) => ArrowCache (Rule m) where
  cache :: forall a b. (Eq a) => Rule m a b -> Rule m a b
  cache (Rule build) = Rule \input -> cacheResult input <$> build input
    where
      cacheResult :: a -> Result m a b -> Result m a b
      cacheResult oldInput Result { rebuild, result } = fix \cachedBuild -> Result
        { rebuild = \newInput -> if
            | oldInput == newInput -> pure cachedBuild
            | otherwise            -> cacheResult newInput <$> rebuild newInput
        , result
        }

class (ArrowChoice arr) => ArrowDistribute arr where
  -- | Given a 'Rule' that operates on key-value pairs, produces a 'Rule' that operates on a
  -- 'M.HashMap'. If the input rule is incremental in its argument, the resulting rule will be
  -- incremental as well for any entries in the map that do not change between builds.
  --
  -- TODO: Laws that capture order-independence.
  --
  -- This is intended to be used as a control operator in @proc@ notation; see
  -- Note [Weird control operator types] in "Control.Arrow.Extended".
  keyed
    :: (Eq k, Hashable k)
    => arr (e, (k, (a, s))) b
    -> arr (e, (M.HashMap k a, s)) (M.HashMap k b)

-- Note that 'ErrorA' does /not/ support an instance of 'ArrowDistribute', as it is impossible to
-- define an instance that short-circuits on the first error! A hypothetical 'ErrorsA' could support
-- such an instance, however, as it could combine all the errors produced by each branch.

instance (Monoid w, ArrowDistribute arr) => ArrowDistribute (WriterA w arr) where
  keyed (WriterA f) = WriterA (arr (swap . sequence . fmap swap) <<< keyed f)

instance (Monad m) => ArrowDistribute (Rule m) where
  keyed
    :: forall a b k e s. (Eq k, Hashable k)
    => Rule m (e, (k, (a, s))) b
    -> Rule m (e, (M.HashMap k a, s)) (M.HashMap k b)
  keyed entryRule = buildWith M.empty
    where
      buildWith
        :: M.HashMap k (Rule m (e, (a, s)) b)
        -> Rule m (e, (M.HashMap k a, s)) (M.HashMap k b)
      buildWith !ruleMap = Rule \(e, (valueMap, s)) ->
        M.traverseWithKey (processEntry e s) valueMap <&> \resultMap -> Result
          { rebuild = build (buildWith (Rule . rebuild <$> resultMap))
          , result = result <$> resultMap
          }
        where
          processEntry :: e -> s -> k -> a -> m (Result m (e, (a, s)) b)
          processEntry e s k v = build (ruleForKey k) (e, (v, s))

          ruleForKey :: k -> Rule m (e, (a, s)) b
          ruleForKey k = case M.lookup k ruleMap of
            Just existingRule -> existingRule
            Nothing           -> lmap (\(e, (v, s)) -> (e, (k, (v, s)))) entryRule

{- Note [Rule rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained by Note [Arrow rewrite rules] in Control.Arrow.Extended, it’s important to define
type-specific rewrite rules to get good performance with arrows. This is especially important for
`Rule`, since the recursive definitions of operations like `.` and `arr` are very difficult for the
optimizer to deal with, and the composition of lots of small rules created with `arr` is very
inefficient.

Fortunately, efficient rules for `Rule` aren’t too hard. The idea is to define all the operations in
terms of a small set of primitives: `.`, `arrM`, `first`, and `left`. Then we can introduce rules
for `arrM` fusion, and the arguments to `arrM` are just plain old monadic actions, which GHC is
really good at optimizing already. This doesn’t get rid of uses of `.` entirely, but it
significantly reduces them.

Since GHC aggressively specializes and inlines class methods, the rules cannot be defined on the
class methods themselves. Instead, the class methods expand to auxiliary definitions, and those
definitions include an INLINABLE[0] pragma that ensures they do not inline until the final
optimization phase. The rules are defined in terms of those definitions, so they will be able to do
their work in prior phases. -}
