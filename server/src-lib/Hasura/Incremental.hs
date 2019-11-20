-- | A simple implementation of /incremental build rules/, which can be used to avoid unnecessary
-- recomputation on incrementally-changing input. See 'Rule' for more details.
module Hasura.Incremental
  ( Rule
  , Result
  , rule
  , build
  , rebuild
  , rebuildRule
  , result

  , mapRule
  , mapRuleS

  , cache
  , cacheWithWriter
  , keyed
  ) where

import           Hasura.Prelude      hiding (id, (.))

import qualified Data.HashMap.Strict as M

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Data.Profunctor

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
-- Atomic rules are created with the 'rule' function, and caching can be added to a rule using the
-- 'cache' combinator. Rules can be executed using the 'build' function, which returns a 'Result'. A
-- 'Result' contains the built value, accessible via 'result', but it also allows supplying a new
-- input value using 'rebuild' to produce a new result incrementally.
newtype Rule m a b
  = Rule { build :: a -> m (Result m a b) }
  deriving (Functor)

-- | Creates a 'Rule' that produces an @b@ from an @a@ using the given monadic function. No caching
-- is applied by default, so the rule will be re-executed on every subsequent rebuild unless it is
-- explicitly wrapped in 'cache'.
rule :: (Functor m) => (a -> m b) -> Rule m a b
rule f = Rule $ \input -> f input <&> \result ->
  Result { rebuild = build (rule f), result }

-- | Modifies a 'Rule' by applying a natural transformation.
mapRule :: (Functor n) => (forall r. m r -> n r) -> Rule m a b -> Rule n a b
mapRule f rule' = Rule \input -> f (build rule' input) <&> \result' ->
  result' { rebuild = build (mapRule f (Rule $ rebuild result')) }

-- | Like 'mapRule', but the transformation can produce an extra piece of state in the result. This
-- is most useful for running monad transformers like 'runWriterT' that accumulate extra information
-- during execution (but note the caveats about caching noted in the documentation for 'cache').
mapRuleS :: (Functor n) => (forall r. m r -> n (r, s)) -> Rule m a b -> Rule n a (b, s)
mapRuleS f rule' = Rule \input -> f (build rule' input) <&> \(Result { rebuild, result }, s) ->
  Result { rebuild = build (mapRuleS f (Rule rebuild)), result = (result, s) }

instance (Applicative m) => Applicative (Rule m a) where
  pure a = Rule . const . pure $ pure a
  rule1 <*> rule2 = Rule $ \input -> liftA2 (<*>) (build rule1 input) (build rule2 input)

instance (Functor m) => Profunctor (Rule m) where
  dimap f g (Rule build) = Rule (fmap (dimap f g) . build . f)

instance (Functor m) => Strong (Rule m) where
  first' (Rule build) = Rule $ \(a, b) -> resultFirst b <$> build a
    where
      resultFirst b Result { rebuild, result } = Result
        { rebuild = \(a, b') -> resultFirst b' <$> rebuild a
        , result = (result, b)
        }

instance (Applicative m) => Choice (Rule m) where
  -- This is significantly trickier to implement than 'first'! Here’s how to think about it: the
  -- first time the rule executes, we know nothing about previous runs, so if we’re given 'Left',
  -- we have to call the original rule we’re given. At that point, as long as we are still given
  -- 'Left' on every rebuild, we can take advantage of whatever caching happened on the previous
  -- run, so we keep recursively calling 'leftResult'.
  --
  -- However, as soon as we get 'Right', we have to bail out. We return the input we’re given, and
  -- we forget about any previous executions of the rule completely. If we’re given 'Left' on a
  -- subsequent rebuild, we start over from the original rule again.
  left' (Rule build) = Rule eitherResult
    where
      eitherResult = either (fmap leftResult . build) rightResult
      leftResult Result { rebuild, result } = Result
        { rebuild = either (fmap leftResult . rebuild) rightResult
        , result = Left result
        }
      rightResult input = pure $ Result
        { rebuild = eitherResult
        , result = Right input
        }

instance (Monad m) => Category (Rule m) where
  id = Rule . fix $ \build -> pure . Result build
  rule2 . rule1 = Rule $ \input -> do
    result1 <- build rule1 input
    result2 <- build rule2 (result result1)
    pure $ Result
      { rebuild = build (Rule (rebuild result2) . Rule (rebuild result1))
      , result = result result2
      }

instance (Monad m) => Arrow (Rule m) where
  arr f = Rule . fix $ \build -> pure . Result build . f
  first = first'

instance (Monad m) => ArrowChoice (Rule m) where
  left = left'

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

-- | Adds equality-based caching to the given rule. After each execution of the rule, its input and
-- result values are cached. On the next rebuild, the input value is compared via '==' to the
-- previous input value. If they are the same, the previous build result is returned /without/
-- re-executing the rule. Otherwise, the old cached values are discarded, and the rule is
-- re-executed to produce a new set of cached values.
--
-- Indescriminate use of 'cache' is likely to have little effect except to increase memory usage,
-- since the input and result of each rule execution must be retained in memory. Avoid using 'cache'
-- around rules with large input or output that is likely to change often unless profiling
-- indicates it is computationally expensive enough to be worth the memory overhead.
--
-- __Note that only direct inputs and outputs of a 'Rule' are cached.__ It is extremely important to
-- take care in your choice of the base monad @m@:
--
--   * Monads that provide access to extra information through a side-channel, such as 'ReaderT',
--     'StateT', or 'IO', will __not__ expose that information to dependency analysis. If that
--     information changes between builds, but the rule’s direct inputs remain unchanged, the rule
--     will __not__ be re-executed.
--
--   * Dually, monads that perform side-effects as part of execution, such as 'StateT', 'WriterT',
--     or 'IO', will __not__ have their side-effects automatically replayed if the cached result is
--     used. If the side effects are only necessary to change some state to bring it in line with
--     the updated inputs, that is entirely fine (and likely even desirable), but if the
--     side-effects are necessary to produce each result, caching will lead to incorrect behavior.
--
-- The safest monad to use for @m@ is therefore 'Identity', which suffers neither of the above
-- problems by construction. However, in practice, it is highly desirable to be able to execute
-- rules that may perform effects such as raising errors, accumulating information, or modifying
-- external state, so the capability is exposed. See also
--
-- See also 'cacheWithWriter' for a variant of 'cache' that cooperates with 'MonadWriter' to allow
-- safe use of accumulative state.
cache :: forall a b m. (Eq a, Applicative m) => Rule m a b -> Rule m a b
cache (Rule build) = Rule $ \input -> cacheResult input <$> build input
  where
    cacheResult :: a -> Result m a b -> Result m a b
    cacheResult oldInput Result { rebuild, result } = fix $ \cachedBuild -> Result
      { rebuild = \newInput -> if
          | oldInput == newInput -> pure cachedBuild
          | otherwise            -> cacheResult newInput <$> rebuild newInput
      , result
      }

-- | Like 'cache', but safe to use with 'MonadWriter'. Any uses of 'tell' during the rule execution
-- will be captured and cached alongside the resulting value, and they will be effectively replayed
-- whenever the cached value is used.
cacheWithWriter :: forall a b m w. (Eq a, MonadWriter w m) => Rule m a b -> Rule m a b
cacheWithWriter (Rule build) = Rule $ \input -> cacheResult input <$> listen (build input)
  where
    cacheResult :: a -> (Result m a b, w) -> Result m a b
    cacheResult oldInput (Result { rebuild, result }, capturedLog) = fix $ \cachedBuild -> Result
      { rebuild = \newInput -> if
          | oldInput == newInput -> tell capturedLog $> cachedBuild
          | otherwise            -> cacheResult newInput <$> listen (rebuild newInput)
      , result
      }

-- | Given a 'Rule' that operates on key-value pairs, produces a 'Rule' that operates on a
-- 'M.HashMap'. If the input rule is incremental in its argument, the resulting rule will be
-- incremental as well for any entries in the map that do not change between builds.
keyed
  :: forall a b k m. (Eq k, Hashable k, Applicative m)
  => Rule m (k, a) b -> Rule m (M.HashMap k a) (M.HashMap k b)
keyed baseRule = buildWith M.empty
  where
    buildWith :: M.HashMap k (Rule m a b) -> Rule m (M.HashMap k a) (M.HashMap k b)
    buildWith !ruleMap = Rule $ \valueMap ->
      M.traverseWithKey processEntry valueMap <&> \resultMap -> Result
        { rebuild = build (buildWith (Rule . rebuild <$> resultMap))
        , result = result <$> resultMap
        }
      where
        processEntry :: k -> a -> m (Result m a b)
        processEntry k v =
          let ruleForKey = case M.lookup k ruleMap of
                Just existingRule -> existingRule
                Nothing           -> lmap (k,) baseRule
          in build ruleForKey v
