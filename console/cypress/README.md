# Test

## Running all tests to generate coverage

<<<<<<< HEAD
Run the command `npm run test` from the `comsole` directory to run all the tests.
=======

1.  Set the `TEST_MODE` field in `cypress.json` to `cli`
2.  Run the command `npm run test` from the `console` directory to run all the tests.

You can also run the complete tests from the cypress electron app:

1.  Set the `TEST_MODE` field in `cypress.json` to `cli`
2.  Run `npm run cypress` from the `console` directory.
3.  Click on `test_complete.js`
    > > > > > > > e0a4ee88feb670d628827a2560c67942471f6046

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

<<<<<<< HEAD

- # Go to the `cypress.json` and set the `env > TEST_MODE` variable to `individual`.
- Go to the `cypress.json` and set the `env > TEST_MODE` variable to `ui`.

  > > > > > > > e0a4ee88feb670d628827a2560c67942471f6046

  ```
  {
      "env": {
  <<<<<<< HEAD
          "TEST_MODE": "individual"
  =======
          "TEST_MODE": "ui"
  >>>>>>> e0a4ee88feb670d628827a2560c67942471f6046
      }
  }
  ```

- Run the command `npm run cypress` and click on `create-tabel > test.js`
