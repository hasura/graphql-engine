# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- A Postgres server (Recommended: Use docker to run a local postgres instance)
- GNU Make (optional)

## Development workflow

### Fork and clone
- Fork the repo on GitHub
- Clone your forked repo: `git clone https://github.com/<your-username>/graphql-engine`

### Compile
- `cd graphql-engine/server`
- `stack build --fast --flag graphql-engine:local-console`
- To enable console for local development, if the folder `../console/node_modules/` is not present

```
  cd ../console
  npm install
  cd ../server
```

### Run
- Make sure postgres is running
- Create a database on postgres
- Run the binary: `stack exec graphql-engine -- --database-url=<database-url> serve`

### Work
- Work on the feature/fix
- Add test cases if relevant

### Test
- Install the py-test dependencies:

```
pip3 install -r tests-py/requirements.txt
```

- Make sure postgres is running
- Run the graphql-engine:

```
stack exec graphql-engine -- --database-url=<database-url> serve`
```

- Set the environmental variables for event-trigger tests

```
export EVENT_WEBHOOK_HEADER="MyEnvValue"
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
```

- Run tests:

```
cd tests-py
pytest -vv --hge-url=http://127.0.0.1:8080 --pg-url=<database_url>
```

### Create Pull Request
- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- Create a pull request from your forked repo to the main repo.
- Every pull request will automatically build and run the tests.

## Code conventions

This helps enforce a uniform style for all committers.

- Compiler warnings are turned on, make sure your code has no warnings.
- Use [hlint](https://github.com/ndmitchell/hlint) to make sure your code has no warnings.
- Use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to format your code.
