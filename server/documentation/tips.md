<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Tips and tricks](#tips-and-tricks)
    - [Use curl and yaml2json to test the graphql-engine API directly](#use-curl-and-yaml2json-to-test-the-graphql-engine-api-directly)
    - [Convert a test case to a live example](#convert-a-test-case-to-a-live-example)
    - [Run a remote MSSQL instance with dev.sh](#run-a-remote-mssql-instance-with-devsh)

<!-- markdown-toc end -->

# Tips and tricks

This document contains various tips and tricks to make development easier.

## Use curl and yaml2json to test the graphql-engine API directly

- [yaml2json](https://github.com/bronze1man/yaml2json) - many of our tests are written in yaml, this utility can convert them to json which graphql-engine understands

Example invocation:

```sh
cat /tmp/metadata.yaml | yaml2json | curl -d @- http://localhost:8181/v1/metadata
```

## Convert a test case to a live example

To manually run an integration test one needs to:

- Run `scripts/dev.sh <postgres|graphql-engine|mssql>`
- import the metadata or connect to DBs as you need
- initialise the MSSQL DB with the raw SQL page
- test the thing via GraphiQL

Fortunately this process can be somewhat automated.
We'll use the `TestGraphQLQueryBasicMSSQL` test as an example.

### Prerequisites

- [yaml2json](#use-curl-and-yaml2json-to-test-the-graphql-engine-api-directly)
- curl

### Start-up graphql-engine

First step stays the same. Start up the relevant databases and graphql-engine in seperate terminals.

We also need mssql for this test, this can be skipped if you're testing postgres for example.

```sh
scripts/dev.sh postgres
```
```sh
scripts/dev.sh mssql
```
```sh
scripts/dev.sh graphql-engine
```

### Connect to a database

In the case of mssql, we also need to register the database. This can be done in the hasura console but going to
the `DATA` tab, then `Manage` button on the left and then `Connect Database` button. Add a mssql database with the
connection string that `scripts/dev.sh mssql` outputed.

Note: the database name should match the `source` field that tests use. In mssql's case this is usually `mssql`.

### Setup schema

The test `TestGraphQLQueryBasicMSSQL` is defined in `server/tests-py/test_graphql_queries.py`.
From there we can learn that the test files are found in the `dir` `server/tests-py/queries/graphql_query/basic`.

For mssql, we are looking for these files:

- `setup_schema_mssql.yaml` - creates tables and inserts values
- `setup_mssql.yaml` - creates relationships, permissions, etc.

And we will run them in that order.

For postgres tests, you will want to run `setup.yaml` and maybe `values_setup.yaml` as well.

We will setup an api call to graphql-engine per setup file:

```sh
cat server/tests-py/queries/graphql_query/basic/schema_setup_mssql.yaml | yaml2json | curl -d @- localhost:8181/v2/query
cat server/tests-py/queries/graphql_query/basic/setup_mssql.yaml | yaml2json | curl -d @- localhost:8181/v1/metadata
```

### Run tests

We have two options:

1. Take the query from the test you like and run in in graphql.
2. Extract the query into a separate file: `/tmp/query.yaml`:
   ```yaml
   query: |
     query {
       author {
         id
         name
       }
     }
   ```
   And use an api call:
   ```sh
   cat /tmp/query.yaml | yaml2json | curl -d @- localhost:8181/v1/graphql
   ```

   To include session variables, use the `-H` curl option:
   ```sh
   cat /tmp/query.yaml | yaml2json | curl -H "X-Hasura-Role: user" -H "X-Hasura-User-Id: 1" -d @- localhost:8181/v1/graphql
   ```

### Cleanup

Easiest way to clean-up is to terminate graphql-engine and the database.

But it is also possible to run the teardown files against graphql-engine. Like this:

```sh
cat server/tests-py/queries/graphql_query/basic/teardown_mssql.yaml | yaml2json | curl -d @- localhost:8181/v1/metadata
cat server/tests-py/queries/graphql_query/basic/schema_teardown_mssql.yaml | yaml2json | curl -d @- localhost:8181/v2/query
```

## Run a remote MSSQL instance with dev.sh

Sometimes we might want to run a database such as MSSQL on a remote computer using `scripts/dev.sh mssql` and connect
to it from `graphql-engine` which runs on a different computer. Currently, mssql instance running using
`scripts/dev.sh mssql` will only be exposed to the machine it is run on.

To change that and expose it to other machines as well, we need to edit `scripts/containers/mssql.sh` and change
the `MSSQL_HOST` variable to the external IP of the machine.
