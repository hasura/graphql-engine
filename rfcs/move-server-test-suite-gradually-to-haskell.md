## Move server test suite (gradually) to Haskell

The Python test suite is useful during CI and for local development, but it can be difficult to maintain for those who are not familiar with Python. As we work on new features, we will need new types of tests, and adding features to the Python code is arguably not the best approach.

### Motivation

Suppose we want to test a table with generated or default columns. Between successive runs of the test suite, values in default columns may change, even for the same inputs. This makes our tests fragile (we have to anticipate the values of default columns and hard-code them into the test suite code), but it also prevents certain feature work: we cannot, for example, include row information in error messages, because then we will not be able to hard-code the error message in the test suite code.

This is not a purely theoretical concern: it is currently holding up work on improved error messages for check constraints (https://github.com/hasura/graphql-engine/issues/3658).

### Proposed change

- Add a new Haskell test suite which uses a similar approach to the current Python test suite, connecting to a Postgres database to set up the schema, making API calls to graphql-engine, and validating their responses.
- Use a library such as `hspec-golden` to implement golden tests in the same style as the current Python test suite.
- Provide a library of functions for writing custom tests which cannot be written as golden tests (such as the example above).
- Migrate tests from Python to Haskell as it becomes necessary, and plan to slowly migrate all tests which Haskell can easily support.

#### Other options considered:

- Why not simply update the Python code to support these new styles of test? For example, to solve the immediate problem, we could introduce some convention in our YAML files for matching strings in responses based on regexes.

  1. Such conventions in our YAML files are undoubtedly a hack, and are reasonably likely to cause issues in future.
  2. This will probably not be the last feature change needed in the Python code, and Haskell is arguably better suited to the creation of a library of composable testing functions.
  3. A test suite in Haskell will be a better place to add unit tests and property-based tests for pure functions, as we refactor more code into a pure core with an effectful outer shell. This way, eventually, we will be able to test the Haskell code without requiring a separate running HGE process.

### Implementation

A prototype test suite is already implemented, although it does not include golden tests. If this approach sounds reasonable, I will work on integrating it into the CI build as a part of a PR for the original issue (https://github.com/hasura/graphql-engine/issues/3658).
