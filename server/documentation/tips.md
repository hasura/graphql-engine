# Tips and tricks

This document contains various tips and tricks to make development easier.

### Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can generate a default version of it with `M-x
markdown-toc-refresh-toc` (provided by the package markdown-toc), and then edit
it for readability.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [Use curl and yaml2json to test the graphql-engine API directly](#use-curl-and-yaml2json-to-test-the-graphql-engine-api-directly)
- [Convert a test case to a live example](#convert-a-test-case-to-a-live-example)
- [Run a remote MSSQL instance with dev.sh](#run-a-remote-mssql-instance-with-devsh)
- [Add a unit test for SQL generation](#add-a-unit-test-for-sql-generation)
- [Benchmark a query on postgres](#benchmark-a-query-on-postgres)
- [Automatically run unit tests on change](#automatically-run-unit-tests-on-change)

<!-- markdown-toc end -->


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
cat server/tests-py/queries/graphql_query/basic/schema_setup.yaml | yaml2json | curl -d @- localhost:8181/v1/query
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
cat server/tests-py/queries/graphql_query/basic/schema_teardown.yaml | yaml2json | curl -d @- localhost:8181/v1/query
```

## Run a remote MSSQL instance with dev.sh

Sometimes we might want to run a database such as MSSQL on a remote computer using `scripts/dev.sh mssql` and connect
to it from `graphql-engine` which runs on a different computer. Currently, mssql instance running using
`scripts/dev.sh mssql` will only be exposed to the machine it is run on.

To change that and expose it to other machines as well, we need to edit `scripts/containers/mssql.sh` and change
the `MSSQL_HOST` variable to the external IP of the machine.

## Add a unit test for SQL generation
We will look at the SQL generation of delete for MSSQL as an example. We want to test the conversion of `AnnDel` to structured SQL.

We can unit test individual transformations, for example that the `Hasura.Backends.MSSQL.FromIr.fromDelete`
function converts an `AnnDel` to the correct SQL `DELETE` statement, like this:

1. Add a new HSpec file in `server/src-test/Database/MSSQL/` named something like `DeleteSpec.hs`:
    - This test should expose a `spec` function with the tests
    - It can use the `shouldBe` or `shouldSatisfy` combinators to compare the input to the expected output
    - We can use `runValidate` and `runFromIr` just as it is used in the codebase to extract the value
2. We can use `ltrace` or similar to print the *input* to `fromDelete` when run from graphql-engine instead of crafting it by hand
3. We can print the output of the function when running the test, or craft the expected output ourselves
4. We need to add another line to `unitSpecs` in `server/src-test/main.hs`

**Note**: it is possible that `Eq` and `Show` instances will need to be added for the input and output types
for this to work (so that hspec can compare the values and display the expected/got mismatches)

### Test example

```hs
module Hasura.Backends.MSSQL.FromIRTest
  ( spec,
  )
where

import Control.Monad.Validate (runValidate)
import Database.ODBC.SQLServer
import Debug.Trace qualified as D
import Hasura.Backends.MSSQL.FromIr
import Hasura.Backends.MSSQL.Types.Internal hiding (FieldName)
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax
import Test.Hspec

spec :: Spec
spec =
  describe "Translate Delete" $
    it "AnnDel to Delete" $ do
      -- Can also be @`shouldBe` Right result@ instead
      runValidate (runFromIr (fromDelete input)) `shouldSatisfy` thing
  where
    thing =
      \case
        Left _ -> False
        Right x -> D.traceShow x True

input :: AnnDel 'MSSQL
input =
  AnnDel
    { dqp1Table = TableName {tableName = "author", tableSchema = "dbo"},
      dqp1Where =
        ( BoolAnd [],
          BoolAnd [...]
        ),
      dqp1Output = MOutMultirowFields [...],
      dqp1AllCols = [...]
    }

result :: Delete
result =
  Delete
    { deleteTable =
        Aliased
          { aliasedThing = TableName {tableName = "author", tableSchema = "dbo"},
            aliasedAlias = "t_author1"
          },
      deleteOutput = Output {...},
      deleteTempTable = TempTable {...},
      deleteWhere = Where [...]
    }
```

See as a commit: https://github.com/hasura/graphql-engine-mono/commit/6fe03938d4255fbba3ec700a8f99527f60d795da
(please completely ignore the `Show` related changes)

## Benchmark a query on postgres

We can measure the performance of a postgres query using the [pgbench](https://www.postgresql.org/docs/current/pgbench.html) tool.
`pgbench` lets us run a query on postgres repeatedly and reports information such as the number of transactions completed in a given time.
We can also compare multiple queries by running each of them using pgbench and compare the results.

### Process

To measure, we need to:

1. Define the schema
2. Generate and insert data
3. Run the query/queries with `pgbench`
4. Compare the results (When comparing multiple queries)

#### Define the schema

This can be done by creating a sql file with the relevant tables for the benchmark. For example:

```sql
-- tables.ddl

drop table if exists author;
drop table if exists article;
CREATE TABLE author(
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);
CREATE TABLE article(
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  author_id INTEGER
);
```

#### Generate data

__Note__: When deciding on the data we want to generate, it is important to remember that the shape of the data, such as its
size and distribution of values, can affect the benchmark results. So make sure you have this in mind when generating the data.

Data for the benchmark can be generated using your favorite programming language. For example, using Haskell:

```hs
-- data-gen.hs

import System.Environment
import Data.Maybe

main = do
  args <- getArgs
  let
    numOfRows = maybe 500 read $ listToMaybe args
    divEveryRows = maybe 80 read $ listToMaybe $ drop 1 args

  putStrLn $ "Number of rows: " <> show numOfRows
  putStrLn $ "Number of unique authors used: " <> show divEveryRows

  let
    sql i = "insert into author(name) values ('Title " <> show i <> "');"
  writeFile "insert_author.sql" $ unlines $ map sql [1..numOfRows]

  let
    sql i = "insert into article(title, author_id) values ('Title " <> show i <> "', " <> show (i `mod` divEveryRows) <> ");"
  writeFile "insert_article.sql" $ unlines $ map sql [1..numOfRows]
```

This snippet above generates sql insert statements for our tables which can be later inserted into postgres.

#### Pgbench

We can measure our query with pgbench with the following (or similar) invocation, which limits the benchmark time (`-T`) to 20 seconds,
and uses a single client (`-c`).

```sh
pgbench -c 1 -T 20 -n -U <username> -d <database> -p 5432 -h 127.0.0.1 -f <queryfile.sql> 2> /dev/null
```

#### One script to rule them all

All of the above steps can be glued together by a simple bash script:

```sh
#!/bin/bash

echo "* Generating data..."
runghc data-gen.hs 1000 200

echo "* Creating schema and inserting data..."
PGPASSWORD=postgres psql -h 127.0.0.1 -p 25432 postgres -U postgres -f tables.ddl > /dev/null
PGPASSWORD=postgres psql -h 127.0.0.1 -p 25432 postgres -U postgres -f insert_author.sql > /dev/null
PGPASSWORD=postgres psql -h 127.0.0.1 -p 25432 postgres -U postgres -f insert_article.sql > /dev/null

echo "* Benchmarking..."
echo ""
echo "-------------------------"
echo "** Query 1: <description>"
echo "-------------------------"
PGPASSWORD=postgres pgbench -c 1 -T 20 -n -U postgres -d postgres -p 25432 -h 127.0.0.1 -f <query_1>.sql 2> /dev/null
echo "-------------------------"
echo "** Query 2: <description>"
echo "-------------------------"
PGPASSWORD=postgres pgbench -c 1 -T 20 -n -U postgres -d postgres -p 25432 -h 127.0.0.1 -f <query_2>.sql 2> /dev/null
```

Which can be run by starting a postgres database with `scripts/dev.sh postgres` and running `bash script.sh`.

#### Compare the results

One simple metric we can use to compare the results of two queries is to look at the transactions rate.
The benchmark which managed to complete more transactions in a specific time frame is often faster.

__Note__: Benchmark results can vary for many reasons, such as the state of the machine and the processes running in parallel. It is important to take that into account when measuring, and considering benchmarking multiple times.

```
---------------------------
** Query 1: Without LIMIT 1
---------------------------
pgbench (14.1, server 12.6)
transaction type: no_limit.sql
scaling factor: 1
query mode: simple
number of clients: 1
number of threads: 1
duration: 20 s
number of transactions actually processed: 6624
latency average = 3.019 ms
initial connection time = 2.930 ms
tps = 331.190445 (without initial connection time)
------------------------
** Query 2: With LIMIT 1
------------------------
pgbench (14.1, server 12.6)
transaction type: with_limit.sql
scaling factor: 1
query mode: simple
number of clients: 1
number of threads: 1
duration: 20 s
number of transactions actually processed: 5822
latency average = 3.435 ms
initial connection time = 3.523 ms
tps = 291.082637 (without initial connection time)
```

From looking at the transactions rate, we can see that `6624` manage to complete for the first query, but only `5822` transactions coleted for the second query.
This makes the first query faster by `6624 / 5822 * 100 - 100 = roughly 13%`. For the usecase and data we measured.

# Automatically run unit tests on change

Use [ghcid](https://hackage.haskell.org/package/ghcid)

And run this:

```sh
ghcid -a -c "cabal repl graphql-engine:graphql-engine-tests -f -O0" --test Main.main --setup ":set args --match TheNameOfTestsIWantToRun" --width 100 --height 30
```
