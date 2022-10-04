# Server testing guidelines

This is a living document and subject to change. You are encouraged to improve
and propose any changes you wish. This document presents the desired state and
might not precisely describe the current state/practices within our codebase.

While most of us will agree that testing is important, it's also very important
to keep a consistent style throughout the server.

This document attempts to serve as a guideline to adding new unit, property, and
integration tests.

## Adding tests

When adding tests, please consider that tests should:
- verify a desired property or test case of the system
- be easy to understand and read
- run fast

See [values](#values) for more details.

### Which kind of test to add?

You should generally favor property tests over unit tests, and unit tests over
integration tests.

We prefer property tests over unit tests because properties are stronger than
examples: a rule will tell you more about how a system behaves than one or a few
particular cases. Generators can come up with examples that we would otherwise
miss.

For an example of property test see [JSONPathSpec](./src-test/Data/Parser/JSONPathSpec.hs).

That being said, there are times where unit tests fit better. Even more often,
property and unit tests complement each other quite well, especially when
hitting certain edge cases proves to be awkward or insufficient with generators.

For an example of using property tests and unit tests together, see
[HashMapSpec](./src-test/Data/HashMap/Strict/ExtendedSpec.hs).

We prefer unit tests over integration tests because they pin-point a specific
function or area of the code, whereas integration tests cover a set of modules
or areas which could even be most of the engine.

For example of unit tests, see [WKTSpec](./src-test/Hasura/SQL/WKTSpec.hs).

For more information on integration tests, see its [README](./lib/api-tests/README.md).

#### Adding property tests

When adding property tests, it might be helpful to add some reasoning about how
you extracted the property you are testing. Often times, this can help clarify
the property. Are there any other related properties you can test?

Secondly, you should consider the generator(s) you are using. Do they
sufficiently cover the common case? What about edge cases? Do they shrink as
well as they can? Do they grow to large enough sizes to handle complex cases?

If you want to read more about property tests, see:
- https://teh.id.au/#/posts/2017/04/23/property-testing-with-hedgehog/
- https://wickstrom.tech/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html

#### Adding unit tests

If property tests do not make sense, then a unit test is the next best thing.
Unit tests should focus, as much as possible, on a specific path in a specific
area of the code: the more precise the better.

We should try to write unit tests such that they are easy to read and
understand even by product experts with little to no experience with Haskell:
- prefer declaring a record type with descriptive field names for related unit
  tests
- define all tests together, such that it's easy to see the important bits at a
  glance (such as, what is being tested, what are the inputs and the expected
  outputs)
- keep the glue (traversal/running test code) out of the spotlight (bottom of
  the modules)

See this PR for a before/after example: https://github.com/hasura/graphql-engine-mono/pull/3418


#### Adding integration tests

Before adding a new integration test, please make sure you're unable to achieve
the same effect through property or unit tests AND that an integration tests
which already covers this scenario does not already exist.

It is recommended that you spend some time thinking about which code changes
might trigger the new integration test to fail and document them: this may
become very valuable to future you or other developers running into test
failures.

A good example of an integration test is [TransactionSpec](./src-test/Database/MSSQL/TransactionSpec.hs).
It is an integration test because it requires a database to be started and
accepting connections. It cannot be run in isolation. However, testing the
behavior of transactions would be a lot more brittle/complicated otherwise.

## Test style

The following sections describe the style for our tests.

### Module naming

The test module names should mimic the same path and name of the module(s) they
are testing, with a prefix describing what kind of test it is. For the module 
`Data.My.Structure`, the module tests should be:
- `Data.My.Structure.UnitSpec` for unit tests
- `Data.My.Structure.PropertySpec` for property tests
- `Data.My.Structure.Gen` for structure generator
- `Data.My.Structure.IntegrationSpec` for integration tests

Note that the `Spec` suffix is required by `hspec-discover`.

### Module organisation

Modules should:
- have an explicit export list in order to make it easier to find the exported
  tests
- exported tests should go first, followed by data types, and helpers

For example:

```haskell
-- | Overview of what this module tests.
module My.Data.StructureTest where
  ( spec
  ) where

import ...

-- | Why are the tests grouped this way?
spec :: Spec
spec = do
  runBasicTests
  runOtherTests

-- | What does this group cover? Any peculiarities?
runBasicTests :: Spec
runBasicTests =
  describe "Basic Structure tests" do
    traverse_ runSort
      [ StructureTest
          { input = [],
            expected = []
          }
      , StructureTest
          { input = [1, 2, 3, 4],
            expected = [1, 2, 3, 4]
          }
      , StructureTest
          { input = [4, 3, 2, 1],
            expected = [1, 2, 3, 4]
          }
      ]

-- | Add documentation if anything has the potential to be surprising.
data StructureTest = StructureTest
  { input :: [Int],
    expected :: [Int],
    description :: String
  }

-- | Add documentation if anything has the potential to be surprising.
runSort :: StructureTest -> Spec
runSort ...
```

### Naming and describing tests

Tests should have a descriptive name, such that when they succeed, we have some
sort of idea what it is they are testing.

Tests should have a clear error, such that, when they fail, we know where to
look and have an idea of what went wrong. See [this PR](https://github.com/hasura/graphql-engine-mono/pull/3748)
as an example of a PR which improves error messages on tests.

### Exported tests

Whenever possible, each module should only export one testing function,
`spec :: Spec` (because that's how [hspec-discover](https://hspec.github.io/hspec-discover.html)
works. If the tests can be split in several categories, then the exported
function should clearly state that in both documentation and implementation.

For example:

```haskell
spec :: Spec
spec = do
  firstGroup
  secondGroup
  -- ...
```

Please do name groups in such a way that they are descriptive to what they are
actually testing, and add documentation as needed.

### Writing property tests

All properties related to a module should go in the top-level `spec` term. The
documentation for `spec` should give an overview of the properties.

Each property should have a comment which explains the property, how does it
generator behave (or reference its documentation), and what kind of guarantees
it provides.

### Writing unit tests

Unit tests should primarily be easy to read. They should clearly convey
information about the test conditions if any, as well as inputs and expected
outputs.

It is recommended that in all but the most trivial cases, we should write down
this information as records with descriptively named fields. For example

```haskell
data UnitTestCaseForSomeFunction = UnitTestCaseForSomeFunction
  { input :: InputType,
    expectation :: ExpectedResultType,
    description :: String
  }
```

It is expected that all test cases are clearly written as a container of such
records, in order to maximize readability.

Please note that sometimes combinators can be more descriptive than records, for
example:

```haskell
[ [1, 2, 3, 4] `shouldSortTo` [1, 2, 3, 4],
  [4, 3, 2, 1] `shouldSortTo` [1, 2, 3, 4]
]
```

However, this is usually the case in trivial cases where we only have one
(simple) input type and a simple output type.

### Writing integration tests

Please refer to its [README](./server/lib/api-tests/README.md) for specific details on how to
add new tests.

## Definitions

A lot of terms this document will be using do not have a strict or exact
definition which is widely accepted at large. The definitions below aim to
clarify their use within this document.

Verifying the correctness of programs and algorithms is a very complicated field
of its own, ranging from formal verification (which usually deals with
rigorously proving the correctness) to manual testing.

In most cases, testing implies comparing expectations about the system (whether
it's the expected result of a computation, a property of the system, or the
formal specification of the system), with the current state of the system
(result of running a computation, result of running several computations, or
even symbolically evaluating the system).

This document will focus on automated testing through unit, property, and
integration tests.

**Unit tests** are automated tests which verify a small part of the system by
specifying the expected result for a certain input.

**Property tests** are automated tests which verify a small part of the system
by specifying a property of the system and allowing a generator to
produce data as input.

**Integration tests** are automated tests which verify a larger part of the
system, usually by specifying the expected result for a certain input.

| Test        | Area  | Input      |
| ----------- | ----- | ---------  |
| unit        | small | manual     |
| property    | small | generator  |
| integration | large | manual (*) |

(*) It is possible to use generators in integration tests, but it's usually not
done because integration tests are slower, and running them multiple times in
succession with inputs from a generator would take too long.

## Values

When adding or modifying existing features, we should do our best to add tests
which verify and support these features. We do so for multiple reasons:
- gain confidence our implementation is correct by writing tests which pass
- allow reviewers to double check their understanding of the changes
- allow us to have more context in the future about this feature/change
- keep us from accidentally breaking this feature

For each of the above, we can think of some metrics:
- our confidence in the correctness of the implementation should come from
  having a reasonably high percentage of coverage, as well as us covering the
  edge cases
- easy to read and understand tests will help both reviewers and future
  colleagues (or us)

On top of these, it is important that the test suite
- runs reasonably fast because we want to run it as often as possible
- has no flaky tests because we want to have confidence in each run
- has easy ways to debug and reproduce test failures

