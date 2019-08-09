## Running tests

The easiest way to run the test suite is to do:

    $ scripts/dev.sh test

This should install python dependencies if required, and run in isolation.

## Tests Structure

- Tests are grouped as test classes in test modules (names starting with `test_`)

- The configuration files (if needed) for the tests in a class are usually kept in one folder.
  - The folder name is usually either the `dir` variable or the `dir()` function


-  Some tests (like in `test_graphql_queries.py`) requires a setup and teardown per class.
   - Here we are extending the `DefaultTestSelectQueries` class.
   - This class defines a fixture which will run the configurations in `setup.yaml` and `teardown.yaml` once per class
   - Extending test class should define a function name `dir()`, which returns the configuration folder


- For mutation tests (like in `test_graphql_mutations.py`)
   - We need a `schema_setup` and `schema_teardown` per class
   - And `values_setup` and `values_teardown` per test
   - Doing schema setup and teardown per test is expensive.
   - We are extending the `DefaultTestMutations` class for this.
   - This class defines a fixture which will run the configuration in `setup.yaml` and `teardown.yaml` once per class.
   - Another fixture defined in this class runs the configuration in `values_setup.yaml` and `values_teardown.yaml` once per class.
