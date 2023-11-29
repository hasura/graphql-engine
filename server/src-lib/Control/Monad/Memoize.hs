{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Memoize
  ( MonadMemoize (..),
    memoize,
    MemoizeT,
    runMemoizeT,
  )
where

import Control.Monad.Except
import Control.Monad.Reader (MonadReader, ReaderT, mapReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DM
import Data.Functor.Identity
import Data.GADT.Compare.Extended
import Data.IORef
import Data.Kind qualified as K
import Language.Haskell.TH qualified as TH
import System.IO.Unsafe (unsafeInterleaveIO)
import Type.Reflection (Typeable, typeRep)
import Prelude

{- Note [Tying the knot]
~~~~~~~~~~~~~~~~~~~~~~~~
GraphQL type definitions can be mutually recursive, and indeed, they quite often
are! For example, two tables that reference one another will be represented by
types such as the following:

    type author {
      id: Int!
      name: String!
      articles: [article!]!
    }

    type article {
      id: Int!
      title: String!
      content: String!
      author: author!
    }

This doesn’t cause any trouble if the schema is represented by a mapping from
type names to type definitions, but the Parser abstraction is all about avoiding
that kind of indirection to improve type safety — parsers refer to their
sub-parsers directly. This presents two problems during schema generation:

  1. Schema generation needs to terminate in finite time, so we need to ensure
     we don’t try to eagerly construct an infinitely-large schema due to the
     mutually-recursive structure.

  2. To serve introspection queries, we do eventually need to construct a
     mapping from names to types (a TypeMap), so we need to be able to
     recursively walk the entire schema in finite time.

Solving point number 1 could be done with either laziness or sharing, but
neither of those are enough to solve point number 2, which requires /observable/
sharing. We need to construct a Parser graph that contains enough information to
detect cycles during traversal.

It may seem appealing to just use type names to detect cycles, which would allow
us to get away with using laziness rather than true sharing. Unfortunately, that
leads to two further problems:

  * It’s possible to end up with two different types with the same name, which
    is an error and should be reported as such. Using names to break cycles
    prevents us from doing that, since we have no way to check that two types
    with the same name are actually the same.

  * Some Parser constructors can fail — the `column` parser checks that the type
    name is a valid GraphQL name, for example. This extra validation means lazy
    schema construction isn’t viable, since we need to eagerly build the schema
    to ensure all the validation checks hold.

So we’re forced to use sharing. But how do we do it? Somehow, we have to /tie
the knot/ — we have to build a cyclic data structure — and some of the cycles
may be quite large. Doing all this knot-tying by hand would be incredibly
tricky, and it would require a lot of inversion of control to thread the shared
parsers around.

To avoid contorting the program, we instead implement a form of memoization. The
MonadMemoize class provides a mechanism to memoize a parser constructor function,
which allows us to get sharing mostly for free. The memoization strategy also
annotates cached parsers with a Unique that can be used to break cycles while
traversing the graph, so we get observable sharing as well. -}

class (Monad m) => MonadMemoize m where
  -- | Memoizes a parser constructor function for the extent of a single schema
  -- construction process. This is mostly useful for recursive parsers;
  -- see Note [Tying the knot] for more details.
  --
  -- The generality of the type here allows us to use this with multiple concrete
  -- parser types:
  --
  -- @
  -- 'memoizeOn' :: ('MonadMemoize' m, MonadParse n) => 'TH.Name' -> a -> m (Parser n b) -> m (Parser n b)
  -- 'memoizeOn' :: ('MonadMemoize' m, MonadParse n) => 'TH.Name' -> a -> m (FieldParser n b) -> m (FieldParser n b)
  -- @
  memoizeOn ::
    forall a p.
    (Ord a, Typeable a, Typeable p) =>
    -- | A unique name used to identify the function being memoized. There isn’t
    -- really any metaprogramming going on here, we just use a Template Haskell
    -- 'TH.Name' as a convenient source for a static, unique identifier.
    TH.Name ->
    -- | The value to use as the memoization key. It’s the caller’s
    -- responsibility to ensure multiple calls to the same function don’t use
    -- the same key.
    a ->
    m p ->
    m p

instance
  (MonadMemoize m) =>
  MonadMemoize (ReaderT a m)
  where
  memoizeOn name key = mapReaderT (memoizeOn name key)

-- | A wrapper around 'memoizeOn' that memoizes a function by using its argument
-- as the key.
memoize ::
  (MonadMemoize m, Ord a, Typeable a, Typeable p) =>
  TH.Name ->
  (a -> m p) ->
  (a -> m p)
memoize name f a = memoizeOn name a (f a)

newtype MemoizeT m a = MemoizeT
  { unMemoizeT :: StateT (DMap MemoizationKey Identity) m a
  }
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader r, MonadTrans)

-- | Allow code in 'MemoizeT' to have access to any underlying state capabilities,
-- hiding the fact that 'MemoizeT' itself is a state monad.
instance (MonadState s m) => MonadState s (MemoizeT m) where
  get = lift get
  put = lift . put

runMemoizeT :: forall m a. (Monad m) => MemoizeT m a -> m a
runMemoizeT = flip evalStateT mempty . unMemoizeT

-- | see Note [MemoizeT requires MonadIO]
instance
  (MonadIO m) =>
  MonadMemoize (MemoizeT m)
  where
  memoizeOn name key buildParser = MemoizeT do
    let parserId = MemoizationKey name key
    parsersById <- get
    case DM.lookup parserId parsersById of
      Just (Identity parser) -> pure parser
      Nothing -> do
        -- We manually do eager blackholing here using a MutVar rather than
        -- relying on MonadFix and ordinary thunk blackholing. Why? A few
        -- reasons:
        --
        --   1. We have more control. We aren’t at the whims of whatever
        --      MonadFix instance happens to get used.
        --
        --   2. We can be more precise. GHC’s lazy blackholing doesn’t always
        --      kick in when you’d expect.
        --
        --   3. We can provide more useful error reporting if things go wrong.
        --      Most usefully, we can include a HasCallStack source location.
        cell <- liftIO $ newIORef Nothing

        -- We use unsafeInterleaveIO here, which sounds scary, but
        -- unsafeInterleaveIO is actually far more safe than unsafePerformIO.
        -- unsafeInterleaveIO just defers the execution of the action until its
        -- result is needed, adding some laziness.
        --
        -- That laziness can be dangerous if the action has side-effects, since
        -- the point at which the effect is performed can be unpredictable. But
        -- this action just reads, never writes, so that isn’t a concern.
        parserById <-
          liftIO $
            unsafeInterleaveIO $
              readIORef cell >>= \case
                Just parser -> pure $ Identity parser
                Nothing ->
                  error $
                    unlines
                      [ "memoize: parser was forced before being fully constructed",
                        "  parser constructor: " ++ TH.pprint name
                      ]
        put $! DM.insert parserId parserById parsersById

        parser <- unMemoizeT buildParser
        liftIO $ writeIORef cell (Just parser)
        pure parser

{- Note [MemoizeT requires MonadIO]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The MonadMemoize instance for MemoizeT requires MonadIO, which is unsatisfying.
The only reason the constraint is needed is to implement knot-tying via IORefs
(see Note [Tying the knot] above), which really only requires the power of
ST. Alternatively, it might be possible to use the ST monad instead, but that
has not been done for historical reasons.
-}

-- | A key used to distinguish calls to 'memoize'd functions. The 'TH.Name'
-- distinguishes calls to completely different parsers, and the @a@ value
-- records the arguments.
data MemoizationKey (t :: K.Type) where
  MemoizationKey :: (Ord a, Typeable a, Typeable p) => TH.Name -> a -> MemoizationKey p

instance GEq MemoizationKey where
  geq
    (MemoizationKey name1 (arg1 :: a1) :: MemoizationKey t1)
    (MemoizationKey name2 (arg2 :: a2) :: MemoizationKey t2)
      | name1 == name2,
        Just Refl <- typeRep @a1 `geq` typeRep @a2,
        arg1 == arg2,
        Just Refl <- typeRep @t1 `geq` typeRep @t2 =
          Just Refl
      | otherwise = Nothing

instance GCompare MemoizationKey where
  gcompare
    (MemoizationKey name1 (arg1 :: a1) :: MemoizationKey t1)
    (MemoizationKey name2 (arg2 :: a2) :: MemoizationKey t2) =
      strengthenOrdering (compare name1 name2)
        `extendGOrdering` gcompare (typeRep @a1) (typeRep @a2)
        `extendGOrdering` strengthenOrdering (compare arg1 arg2)
        `extendGOrdering` gcompare (typeRep @t1) (typeRep @t2)
        `extendGOrdering` GEQ
