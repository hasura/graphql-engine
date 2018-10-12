# Test

## Running all tests to generate coverage

1.  Set the `TEST_MODE` field in `cypress.json` to `cli`
2.  Run the command `npm run test` from the `console` directory to run all the tests.

You can also run the complete tests from the cypress electron app:

1.  Set the `TEST_MODE` field in `cypress.json` to `cli`
2.  Run `npm run cypress` from the `console` directory.
3.  Click on `test_complete.js`

## Running tests individually

Tests are modularized into following modules:

- API-Explorer
- Data
  - Migration Mode
  - Create Table
  - Insert Browse
  - Modify Table
  - Table Relationships
  - Table and View Permissions
  - Views

To run the tests for the modules individually (say for create table),

- Go to the `cypress.json` and set the `env > TEST_MODE` variable to `ui`.

```
{
    "env": {
        "TEST_MODE": "ui"
    }
}
```

- Run the command `npm run cypress` and click on `create-table > test.js`
