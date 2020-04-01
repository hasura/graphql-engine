# Code conventions and style guide

This is a short document describing the preferred coding style for this project. We've tried to cover the major areas of formatting and naming. When something isn't covered by this guide, err on the side of consistency with existing code.

### Formatting

Use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to format your code.

#### Line Length

Try to keep a line length within 100 characters. Small exceptions are okay if the line only goes over the limit by a few characters and breaking it would negatively impact readability. Use your best judgement.

#### Whitespace

Do not use tabs. Use spaces for indenting. Indent your code blocks with **2
spaces**. Indent the `where` keyword two spaces to set it apart from the rest of
the code and indent the definitions in a `where` clause 2 spaces. Some examples:

```haskell
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []   = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

Include newlines at the ends of files. Do not include other trailing whitespace on the end of lines.

#### Data Declarations

Align the constructors in a data type definition. Example:

```haskell
data Tree a
  = Branch !a !(Tree a) !(Tree a)
  | Leaf
```

```haskell
data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
  { firstName :: !String  -- ^ First name
  , lastName  :: !String  -- ^ Last name
  , age       :: !Int     -- ^ Age
  } deriving (Eq, Show)
```

#### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Optionally, you can skip the first newline.  Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

#### Export Lists

Format export lists as follows:

```haskell
module Data.Set
  ( -- * The @Set@ type
    Set
  , empty
  , singleton

    -- * Querying
  , member
  ) where
```

### Imports

Imports should be grouped in the following order:

1. `Hasura.Prelude`
2. `qualified` imports of standard library / third party imports
3. unqualified standard library imports / related third party imports
4. `qualified` imports of local application/library specific imports
5. unqualified local application/library specific imports

Put a blank line between each group of imports. The imports in each group should
be sorted alphabetically, by module name (this is done by `stylish-haskell`
automatically).

### Naming

Use camel case (e.g. `functionName`) when naming functions and upper camel case
(e.g. `DataType`) when naming data types.

Prefer full words over abbreviations. For example, prefer the name `ColumnType`
to `ColTy` and `computeAggregateFunction` to `compAggFn`. Exceptions: extremely
common, idiomatic abbreviations, like using “err” to mean “error” or “ref” to
mean “reference.” When in doubt about whether or not an abbreviation is
well-known, prefer the full name.

#### Functions and variables

Avoid “Hungarian notation,” aka using short variable prefixes or suffixes based
on the variable type. For example, do not prefix or suffix variables or
functions with the letter “m” to indicate that they are or return `Maybe`
values:

```haskell
-- Bad
parseArgumentM :: Maybe UserRole -> Value -> Maybe Argument
parseArgumentM userRoleM value = case userRoleM of
  Just userRole -> ...
  Nothing -> ...
```

Instead, where possible, prefer names that convey *why* the value is wrapped in
a `Maybe`, or, if `Nothing` is just an ordinary member of the value’s domain,
don’t include any special indication in the name at all:

```haskell
-- Good
parseOptionalArgument :: Maybe UserRole -> Value -> Maybe Argument
parseOptionalArgument userRole value = case userRole of
  Just knownUserRole -> ...
  Nothing -> ...
```

When possible, avoid the need to name intermediate results at all so that
coming up with several different variants of the same name is unnecessary. For
example, instead of writing

```haskell
-- Bad
argumentM <- parseOptionalArgument userRole value
case argumentM of
  Just argument -> ...
  Nothing -> ...
```

prefer writing

```haskell
-- Good
parseOptionalArgument userRole value >>= \case
  Just argument -> ...
  Nothing -> ...
```

which avoids the need for the extra name entirely. If such an approach isn’t
practical or is hard to read, and a type-related prefix or suffix is absolutely
necessary, prefer a prefix with the full name of the type over abbreviations:

```haskell
-- Okay
maybeArgument <- parseOptionalArgument userRole value
doSomethingElse
case maybeArgument of
  Just argument -> ...
  Nothing -> ...
```

#### Modules

Use singular, not plural, module names, e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

### Data types

#### Wrap primitives in newtype (or sum types)

It is usually a good idea to introduce a custom data type (sum types or
`newtype`) instead of using a primitive type (like `Int`, `String`, `Set`
`Text`, etc.). This prevents confusion and mistakes of using the wrong types in
wrong places.

#### Prefer sum types to `Bool`

Avoid using `Bool` to represent two different states, especially if the set of possible states could potentially grow in the future. For example, instead of writing

```haskell
-- bad
data LiveQuery = LiveQuery
  { ...
  , lqIsPaused :: !Bool
  , ...
  }
```

prefer a sum type that has meaning even without context:

```haskell
-- good
data LiveQuery = LiveQuery
  { ...
  , lqState :: !LiveQueryState
  , ...
  }

data LiveQueryState
  = LQActive
  | LQPaused
```

Also, avoid defining boolean predicates that simply select distinguish between different constructors of a sum type. For example, avoid this:

```haskell
-- bad
isPaused :: LiveQueryState -> Bool
isPaused LQPaused = True
isPaused _        = False

...

unless (isPaused queryState) do
  handleActiveQuery query
```

This pattern can easily bite you if you later decide to extend the sum type with a new constructor. For example, suppose `LiveQueryState` was extended with an `LQStopped` constructor. Our guard using `isPaused` would still compile, but it would return `False` for stopped queries, and we’d call `handleActiveQuery` even though the query is stopped!

Pattern-matching is much more robust to this kind of code evolution, and it isn’t very much more code:

```haskell
-- good
case queryState of
  LQActive -> handleActiveQuery query
  LQPaused -> pure ()
```

### Dealing with laziness

By default, use strict data types and lazy functions.

#### Data types

Constructor fields should be strict, unless there's an explicit reason to make
them lazy. This avoids many common pitfalls caused by too much laziness and
reduces the number of brain cycles the programmer has to spend thinking about
evaluation order.

```haskell
-- Good
data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
```

```haskell
-- Bad
data Point = Point
  { pointX :: Double
  , pointY :: Double
  }
```

### Functions

Have function arguments be lazy unless you explicitly need them to be strict.

The most common case when you need strict function arguments is in recursion
with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

(Though the above function could be written using `foldl'` instead, which would
be preferable.)

### Comments and documentation

#### Formatting

Use line comments (comments that start with `--`) for short comments (1–3
lines). For longer comments, use multiline comments (comments that begin with
`{-` and end with `-}`).

Use [Haddock syntax](https://haskell-haddock.readthedocs.io/en/latest/markup.html) for documentation comments. Running Haddock should always complete successfully.

#### Module headers

Include a short purpose statement at the top of each module, usually about
1–3 sentences long. Try to describe how the contents of the module relate to
each other (i.e. what criteria determine which bindings should go in this
module?) and to other modules in the project (i.e. what high-level problem do
the exports of this module solve?).

Examples:

```haskell
-- | Types and functions for generating a GraphQL schema from information
-- about Postgres tables.
module Hasura.GraphQL.Context where
```

```haskell
-- | Top-level HTTP routes and handlers for the server, provided as a WAI
-- application.
module Hasura.Server.App where
```

Certain modules may benefit from longer comments, which can serve as
internal documentation for high-level architectural design choices that don’t
make sense to document anywhere else. For example, the module comment for
`Hasura.RQL.DDL.Schema` describes in a couple paragraphs the high-level
concepts behind the schema cache and metadata catalog. As appropriate, link to
that module documentation in other comments using the Haddock syntax of
enclosing a module name in double quotes; for example:

```haskell
-- | Types that represent the raw data stored in the catalog. See also the
-- module documentation for "Hasura.RQL.DDL.Schema".
module Hasura.RQL.Types.Catalog
```

Not every module needs documentation comment—it doesn’t make sense for small
helper/utility modules, for example—but most modules should have one. A module
that cannot be given a short, precise purpose statement may benefit from being
broken up into multiple modules.

#### Functions and datatypes

Include documentation comments on function and datatype declarations to
communicate additional context about their purpose or usage information, but
avoid using comments as a replacement for precise names. For example, instead
of writing

```haskell
-- Bad

-- | Configuration options given on the command-line.
data Options = ...
```

prefer a more self-documenting name:

```haskell
-- Good
data CommandLineOptions = ...
```

Likewise, avoid using comments to describe invariants of a function argument
or datatype field instead of a more precise type:

```haskell
-- Bad
newtype AuthenticationToken
  = AuthenticationToken
  { unAuthenticationToken :: Text
  -- ^ Invariant: contains Base64-encoded binary data.
  }
```
```haskell
-- Good
import Data.Text.Conversions (Base64)
newtype AuthenticationToken
  = AuthenticationToken
  { unAuthenticationToken :: Base64 ByteString
  }
```

However, *do* use comments on functions and datatypes to clarify information
that cannot easily be communicated via names or types:

```haskell
-- Good
resolveTableReference
  :: OMap SchemaName TableName
  -- ^ All tables visible to the current user. Schemas with priority in the
  -- search path should come earlier.
  -> Maybe SchemaName -> TableName -> Maybe QualifiedTable
resolveTableReference = ...
```

#### Inline comments

Use inline comments within function bodies to provide additional context about
why a function does something a particular way or to call special attention to
an easily-overlooked subtlety. For example:

```haskell
-- Good
allComparableTypes
  -- due to casting, we need to generate both geometry and geography operations
  -- even if just one of the two appears directly in the table schema
  | anyGeoTypes = geoTypes <> columnTypes
  | otherwise = columnTypes
```

Include links to external resources such as specifications or GitHub issues
that provide context for decisions:

```haskell
-- Good
isValidEnumName name =
  -- see https://graphql.github.io/graphql-spec/June2018/#EnumValue
  isValidName name && name `notElem` ["true", "false", "null"]
```

Avoid comments that state the obvious (i.e. just restate types or variable
names), and prefer using informative variable or function names over comments
where possible. For example:

```haskell
-- Bad
runSelectQuery tables constraints cache shouldPrepare = do
  -- part 1: construct the query
  select <- buildSelect tables
  conditions <- traverse buildCondition constraints
  query <- buildQuery select conditions

  -- part 2: execute the query
  plan <- cacheLookup query (generateQueryPlan query) cache
  -- prepare if necessary
  prepared <- if shouldPrepare then prepareQueryPlan plan else pure plan
  runQueryPlan prepared
```
```haskell
runSelectQuery tables constraints cache shouldPrepare =
  constructQuery >>= executeQuery
  where
    constructQuery = do
      select <- buildSelect tables
      conditions <- traverse buildCondition constraints
      query <- buildQuery select conditions

    executeQuery = do
      plan <- cacheLookup query (generateQueryPlan query) cache
      prepared <- if shouldPrepare then prepareQueryPlan plan else pure plan
      runQueryPlan prepared
```

#### Out-of-line `Note`s

For especially tricky details that deserve thorough explanation and may need to be referenced from multiple places, we emulate [GHC’s Notes](https://www.stackbuilders.com/news/the-notes-of-ghc). A `Note` is an out-of-line comment written at the top-level of a module, written with a short title header:

```haskell
{- Note [Checking metadata consistency in run_sql]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SQL queries executed by run_sql may change the Postgres schema in arbitrary
ways. We attempt to automatically update the metadata to reflect those changes
as much as possible---for example, if a table is renamed, we want to update the
metadata to track the table under its new name instead of its old one.

... -}
```

At any point where the comment is relevant, we add a short comment referring to the note:

```haskell
-- see Note [Checking metadata consistency in run_sql]
containsDDLKeyword :: Text -> Bool
containsDDLKeyword = TDFA.match $$(quoteRegex ...)
```

A key advantage of notes is that they can be referenced from multiple places, so information does not necessarily need to be connected to any particular binding the way it must be for Haddock comments.

When updating a piece of code that includes a reference to a `Note`, take care to ensure the `Note` is updated as well if necessary! Inevitably, some will get stale and go out of sync with the code, but that’s okay—just fix them up when you find some information is outdated.

### Misc

#### Point-free style

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Acknowledgement/Credits

Parts of this coding style guide have been adapted from:
- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
- https://kowainik.github.io/posts/2019-02-06-style-guide
- https://chrisdone.com/posts/german-naming-convention/
