# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- A Postgres server (Recommended: Use docker to run a local postgres instance)
- GNU Make (optional)
- [Node.js](https://nodejs.org/en/) (v8.9+)
- libpq-dev
- psql
- python >= 3.7 with pip3

## Upgrading npm

If your npm is too old  (< 5.7),

npm install -g npm@latest

or

sudo npm install -g npm@latest

or update your nodejs

## Getting pip3

sudo apt install python3-pip

## Development workflow

### Fork and clone
- Fork the repo on GitHub
- Clone your forked repo: `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine`

### Compile
- compile console assets
  ```
  cd console
  npm ci
  npm run server-build
  cd ..
  ```
- compile the server
  ```
  cd server
  stack build --fast
  ```

### Run
- Make sure postgres is running (Postgres >= 9.5)
- Create a database on postgres
- Run the binary: `stack exec graphql-engine -- --database-url=<database-url> serve`

Use `--enable-console --console-assets-dir ../console/static/dist` if you want console to be served.

database url looks like: `postgres://<username>:<password>@<host>:<port>/<dbname>`

### Running Postgres

The easiest way is to run docker in a container

````
docker run -p 5432:5432 -d postgres:11.1
````

Test if it's running by

telnet localhost 5432

### psql

You will need psql or another client

````
sudo apt install postgresql-client
````


### Work
- Work on the feature/fix
- Add test cases if relevant

### Test
- Install the py-test dependencies:

```
pip3 install -r tests-py/requirements.txt
```

- Make sure postgres is running
- Set the environmental variables for event-trigger tests and run the graphql-engine:

```
export EVENT_WEBHOOK_HEADER="MyEnvValue"
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
stack exec graphql-engine -- --database-url=<database-url> serve --enable-console --stringify-numeric-types
```

- Run tests:

```
cd tests-py
pytest --hge-urls http://127.0.0.1:8080 --pg-urls <database_url> -vv
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
