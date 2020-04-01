## Code conventions and style guide

This helps enforce a uniform style for all committers.

- Compiler warnings are turned on, make sure your code has no warnings.
- Use [hlint](https://github.com/ndmitchell/hlint) to make sure your code has no
  warnings.
- Use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to format
  your code.

This is a short document describing the preferred coding style for this project.
We've tried to cover the major areas of formatting and naming. When something
isn't covered by this guide you should stay consistent with the code in the
other modules.

### Formatting

#### Line Length

Try to keep a line length within 100 characters. Small exceptions are okay if
the line only goes over the limit by a few characters and breaking it would
negatively impact readability; use your best judgement.

#### Indentation

Tabs are illegal. Use spaces for indenting. Indent your code blocks with *2
spaces*. Indent the `where` keyword two spaces to set it apart from the rest of
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

#### Blank Lines

One blank line between top-level definitions. No blank lines between type
signatures and definitions of a function. Add one blank line between functions
in a type class instance declaration if the function bodies are large. Use your
judgement.

#### Whitespace

Surround binary operators with a single space on either side. Use your better
judgement for the insertion of spaces around arithmetic operators but always be
consistent about whitespace on either side of a binary operator. Don't insert a
space after a lambda.

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
data Person
  = Person
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
  (
    -- * The @Set@ type
    Set
  , empty
  , singleton

    -- * Querying
  , member
  ) where
```

### Imports

Imports should be grouped in the following order:

1. standard library imports / related third party imports
2. qualified imports of standard library / third party imports
3. local application/library specific imports
4. qualified imports of local application/library specific imports

Put a blank line between each group of imports. The imports in each group should
be sorted alphabetically, by module name (this is done by stylish-haskell
automatically).

Always use explicit import lists or `qualified` imports for standard and third
party libraries. This makes the code more robust against changes in these
libraries. Exception: The Prelude.

### Naming

Use camel case (e.g. `functionName`) when naming functions and upper camel case
(e.g. `DataType`) when naming data types.

Prefer full words over abbreviations. For example, prefer the name `ColumnType`
to `ColTy` and `computeAggregateFunction` to `compAggFn`. Exceptions: extremely
common, idiomatic abbreviations, like using “err” to mean “error” or “ref” to
mean “reference.” When in doubt about whether or not an abbreviation is
well-known, prefer the full name.

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of `HTTPServer`.
Exception: Two letter abbreviations, e.g. `IO`.

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
feasible or is hard to read, and a type-related prefix or suffix is absolutely
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

### Standard library usage

#### Don't use `unless`

Use `when` combined with `not`. Usage of `unless` can be confusing.

### Data types

#### Wrap primitives in newtype (or sum types)

It is usually a good idea to introduce a custom data type (sum types or
`newtype`) instead of using a primitive type (like `Int`, `String`, `Set`
`Text`, etc.). This prevents confusion and mistakes of using the wrong types in
wrong places.

#### Use sum types instead `Bool` where it makes sense

To represent two different states, use sum types rather than a `Bool`.
Specially, in the case of data types which represents enabled/disabled state.
The problem with `Bool` is, it is not clear what the default is.

Example:

```haskell
data ConsoleEnabled
  = CEDisabled
  | CEEnabled
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
data Point
  = Point
  { pointX :: !Double
  , pointY :: !Double
  }
```

```haskell
-- Bad
data Point
  = Point
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

Use [Haddock syntax](https://haskell-haddock.readthedocs.io/en/latest/markup.html)
for documentation comments. Running Haddock via `stack haddock` should always
complete successfully.

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

### Misc

#### Point-free style

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

#### Warnings

Code should be compilable with `-Wall -Werror`. There should be no warnings.


### Acknowledgement/Credits
Parts of this coding style guide has been adapted from
- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
- https://kowainik.github.io/posts/2019-02-06-style-guide
- https://chrisdone.com/posts/german-naming-convention/
