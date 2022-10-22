-- | Classes for monads used during schema construction and query parsing.
module Hasura.GraphQL.Parser.Class
  ( MonadSchema (..),
    memoize,
    MonadParse (..),
    parseError,
  )
where

import GHC.Stack (HasCallStack)
import Hasura.GraphQL.Parser.Class.Parse
import Hasura.GraphQL.Parser.Internal.Types
import Hasura.Prelude
import Language.Haskell.TH qualified as TH
import Type.Reflection (Typeable)

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
  --
  -- The generality of the type here allows us to use this with multiple concrete
  -- parser types:
  --
  -- @
  -- 'memoizeOn' :: 'MonadSchema' n m => 'TH.Name' -> a -> m (Parser n b) -> m (Parser n b)
  -- 'memoizeOn' :: 'MonadSchema' n m => 'TH.Name' -> a -> m (FieldParser n b) -> m (FieldParser n b)
  -- @
  memoizeOn ::
    forall p a b.
    (HasCallStack, Ord a, Typeable p, Typeable a, Typeable b) =>
    -- | A unique name used to identify the function being memoized. There isn’t
    -- really any metaprogramming going on here, we just use a Template Haskell
    -- 'TH.Name' as a convenient source for a static, unique identifier.
    TH.Name ->
    -- | The value to use as the memoization key. It’s the caller’s
    -- responsibility to ensure multiple calls to the same function don’t use
    -- the same key.
    a ->
    m (p n b) ->
    m (p n b)

-- | A wrapper around 'memoizeOn' that memoizes a function by using its argument
-- as the key.
memoize ::
  (HasCallStack, MonadSchema n m, Ord a, Typeable a, Typeable b, Typeable k) =>
  TH.Name ->
  (a -> m (Parser k n b)) ->
  (a -> m (Parser k n b))
memoize name f a = memoizeOn name a (f a)
