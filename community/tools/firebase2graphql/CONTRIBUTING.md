# Contributing to firebase2graphql

## Issues

Please open an issue related to your work. Add the label `c/firebase2graphql`.

## Local developmet

1. Make changes and save
2. Run the executable in the `bin` directory to test your code. Treat the executable as the command. For example:

   ```
   $ bin/run --help
   ```

## Testing

Please run the tests before making pull requests.

To run the tests locally, you will need an instance of [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) running. To run the tests, run the command:

```
$ TEST_HGE_URL=https://hge.herokuapp.com npm test
```

### Test data sets

Firebase RTD being a NoSQL database, there are very few data-sets available on the web. Since the tool takes heuristic based approach to convert the data, the more data we have the better results we can achieve. If you're aware of any such data-sets, please consider adding them to the test suite (test/data-sets).

