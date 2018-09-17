# Contributing to JSON Data Import

## Issues

Please open an issue related to your work. Add the label `c/json2graphql`.

## Local developmet

1. Make changes and save
2. Run the executable in the `bin` directory to test your code. Treat the executable as the command. For example:

   ```
   $ bin/run --help
   ```

## Testing

Please make sure you run the tests before making pull requests. All the pull requests will be run through tests before merging.

To run the tests locally, you will need an instance of [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) running. To run the tests, run the command:

```
$ TEST_HGE_URL=https://hge.herokuapp.com npm test
```
