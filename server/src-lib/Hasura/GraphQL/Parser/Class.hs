-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as Map
import qualified Language.Haskell.TH                   as TH

import           Data.Has
import           Data.Parser.JSONPath
import           Data.Tuple.Extended
import           GHC.Stack                             (HasCallStack)
import           Type.Reflection                       (Typeable)

import {-# SOURCE #-} Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Table                (TableCache, TableInfo)
import           Hasura.Session                        (RoleName)
import           Hasura.SQL.Types

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
MonadSchema class provides a mechanism to memoize a parser constructor function,
which allows us to get sharing mostly for free. The memoization strategy also
annotates cached parsers with a Unique that can be used to break cycles while
traversing the graph, so we get observable sharing as well. -}

-- | A class that provides functionality used when building the GraphQL schema,
-- i.e. constructing the 'Parser' graph.
class (Monad m, MonadParse n) => MonadSchema n m | m -> n where
  -- | Memoizes a parser constructor function for the extent of a single schema
  -- construction process. This is mostly useful for recursive parsers;
  -- see Note [Tying the knot] for more details.
  memoizeOn
    :: (HasCallStack, Ord a, Typeable a, Typeable b, Typeable k)
    => TH.Name
    -- ^ A unique name used to identify the function being memoized. There isn’t
    -- really any metaprogramming going on here, we just use a Template Haskell
    -- 'TH.Name' as a convenient source for a static, unique identifier.
    -> a
    -- ^ The value to use as the memoization key. It’s the caller’s
    -- responsibility to ensure multiple calls to the same function don’t use
    -- the same key.
    -> m (Parser k n b) -> m (Parser k n b)

type MonadRole r m = (MonadReader r m, Has RoleName r)

-- | Gets the current role the schema is being built for.
askRoleName
  :: MonadRole r m
  => m RoleName
askRoleName = asks getter

type MonadTableInfo r m = (MonadReader r m, Has TableCache r, MonadError QErr m)

-- | Looks up table information for the given table name. This function
-- should never fail, since the schema cache construction process is
-- supposed to ensure all dependencies are resolved.
askTableInfo
  :: MonadTableInfo r m
  => QualifiedTable
  -> m TableInfo
askTableInfo tableName = do
  tableInfo <- asks $ Map.lookup tableName . getter
  -- This should never fail, since the schema cache construction process is
  -- supposed to ensure that all dependencies are resolved.
  tableInfo `onNothing` throw500 ("askTableInfo: no info for " <>> tableName)

-- | A wrapper around 'memoizeOn' that memoizes a function by using its argument
-- as the key.
memoize
  :: (HasCallStack, MonadSchema n m, Ord a, Typeable a, Typeable b, Typeable k)
  => TH.Name
  -> (a -> m (Parser k n b))
  -> (a -> m (Parser k n b))
memoize name f a = memoizeOn name a (f a)

memoize2
  :: (HasCallStack, MonadSchema n m, Ord a, Ord b, Typeable a, Typeable b, Typeable c, Typeable k)
  => TH.Name
  -> (a -> b -> m (Parser k n c))
  -> (a -> b -> m (Parser k n c))
memoize2 name = curry . memoize name . uncurry

memoize3
  :: ( HasCallStack, MonadSchema n m, Ord a, Ord b, Ord c
     , Typeable a, Typeable b, Typeable c, Typeable d, Typeable k )
  => TH.Name
  -> (a -> b -> c -> m (Parser k n d))
  -> (a -> b -> c -> m (Parser k n d))
memoize3 name = curry3 . memoize name . uncurry3

memoize4
  :: ( HasCallStack, MonadSchema n m, Ord a, Ord b, Ord c, Ord d
     , Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable k )
  => TH.Name
  -> (a -> b -> c -> d -> m (Parser k n e))
  -> (a -> b -> c -> d -> m (Parser k n e))
memoize4 name = curry4 . memoize name . uncurry4

-- | A class that provides functionality for parsing GraphQL queries, i.e.
-- running a fully-constructed 'Parser'.
class Monad m => MonadParse m where
  withPath :: (JSONPath -> JSONPath) -> m a -> m a
  -- | Not the full power of 'MonadError' because parse errors cannot be
  -- caught.
  parseError :: Text -> m a
  -- | See 'QueryReusability'.
  markNotReusable :: m ()

-- | Tracks whether or not a query is /reusable/. Reusable queries are nice,
-- since we can cache their resolved ASTs and avoid re-resolving them if we
-- receive an identical query. However, we can’t always safely reuse queries if
-- they have variables, since some variable values can affect the generated SQL.
-- For example, consider the following query:
--
-- > query users_where($condition: users_bool_exp!) {
-- >   users(where: $condition) {
-- >     id
-- >   }
-- > }
--
-- Different values for @$condition@ will produce completely different queries,
-- so we can’t reuse its plan (unless the variable values were also all
-- identical, of course, but we don’t bother caching those).
data QueryReusability = Reusable | NotReusable

instance Semigroup QueryReusability where
  NotReusable <> _           = NotReusable
  _           <> NotReusable = NotReusable
  Reusable    <> Reusable    = Reusable

instance Monoid QueryReusability where
  mempty = Reusable
