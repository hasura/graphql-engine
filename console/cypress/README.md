# Test

## Running all tests to generate coverage

Run the command `npm run test` from the `app` directory to run all the tests.

## Running tests individually

Tests are modularized into following modules:

- Migration Mode
- Create Table
- Insert Browse
- Modify Table
- Table Relationships
- Table and View Permissions
- Views

To run the tests for the modules individually (say for create table),

- Go to the `integration/data/create-table/test.js`
- Uncomment the bottom two lines.

```
setup();
runCreateTableTests;
```

- Run the command `npm run cypress` and click on `create-tabel > test.js`
