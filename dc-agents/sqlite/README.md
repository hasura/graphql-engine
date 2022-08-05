# Data Connector Agent for SQLite

This directory contains an SQLite implementation of a data connector agent.
It can use local SQLite database files as referenced by the "db" config field.

## Capabilities

The SQLite agent currently supports the following capabilities:

* [x] GraphQL Schema
* [x] GraphQL Queries
* [ ] GraphQL Mutations
* [x] Relationships
* [x] Aggregations
* [ ] Exposing Foreign-Key Information
* [ ] Subscriptions
* [ ] Streaming Subscriptions

Note: You are able to get detailed metadata about the agent's capabilities by
`GET`ting the `/capabilities` endpoint of the running agent.

## Requirements

* NodeJS 16
* SQLite `>= 3.38.0` or compiled in JSON support
    * Required for the json_group_array() and json_group_object() aggregate SQL functions
    * https://www.sqlite.org/json1.html#jgrouparray

## Build & Run

```sh
npm install
npm run build
npm run start
```

Or a simple dev-loop via `entr`:

```sh
echo src/**/*.ts | xargs -n1 echo | DB_READONLY=y entr -r npm run start
```

## Options / Environment Variables

* ENV: `PORT=[INT]` - Port for agent to listen on. 8100 by default.
* ENV: `PERMISSIVE_CORS={1|true|yes}` - Allows all requests - Useful for testing with SwaggerUI. Turn off on production.
* ENV: `DB_CREATE={1|true|yes}` - Allows new databases to be created, not permitted by default.
* ENV: `DB_READONLY={1|true|yes}` - Makes databases readonly, they are read-write by default.
* ENV: `DB_ALLOW_LIST=DB1[,DB2]*` - Restrict what databases can be connected to.
* ENV: `DB_PRIVATECACHE` - Keep caches between connections private. Shared by default.
* ENV: `DEBUGGING_TAGS` - Outputs xml style tags in query comments for deugging purposes.

## Agent usage

The agent is configured as per the configuration schema.

The only required field is `db` which specifies a local sqlite database to use.

The schema is exposed via introspection, but you can limit which tables are referenced by

* Explicitly enumerating them via the `tables` field, or
* Toggling the `include_sqlite_meta_tables` to include or exclude sqlite meta tables.


## Docker Build & Run

```
> docker build . -t dc-sqlite-agent:latest
> docker run -it --rm -p 8100:8100 dc-sqlite-agent:latest
```

You will want to mount a volume with your database(s) so that they can be referenced in configuration.

## Dataset

The dataset used for testing the reference agent is sourced from:

* https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sql

## Testing Changes to the Agent

Run:

```sh
cabal run graphql-engine:test:tests-dc-api -- test --agent-base-url http://localhost:8100 --agent-config '{"db": "db.chinook2.sqlite"}'
```

From the HGE repo.

## TODO

* [ ] Pull reference types from a package rather than checked-in files
* [x] Health Check
* [x] DB Specific Health Checks
* [x] Schema
* [x] Capabilities
* [x] Query
* [x] Array Relationships
* [x] Object Relationships
* [x] Ensure everything is escaped correctly - https://sequelize.org/api/v6/class/src/sequelize.js~sequelize#instance-method-escape
* [ ] Or... Use parameterized queries if possible - https://sequelize.org/docs/v6/core-concepts/raw-queries/#bind-parameter
* [x] Run test-suite from SDK
* [x] Remove old queries module
* [x] Relationships / Joins
* [x] Rename `resultTT` and other badly named types in the `schema.ts` module
* [x] Add ENV Variable for restriction on what databases can be used
* [x] Update to the latest types
* [x] Port back to hge codebase as an official reference agent
* [x] Make escapeSQL global to the query module
* [x] Make CORS permissions configurable
* [x] Optional DB Allowlist
* [x] Fix SDK Test suite to be more flexible about descriptions
* [x] READONLY option
* [x] CREATE option
* [x] Don't create DB option
* [x] Aggregate queries
* [x] Verbosity settings
* [x] Cache settings
* [x] Missing WHERE clause from object relationships
* [x] Reuse `find_table_relationship` in more scenarios
* [x] ORDER clause in aggregates breaks SQLite parser for some reason
* [x] Check that looped exist check doesn't cause name conflicts
* [ ] `NOT EXISTS IS NULL` != `EXISTS IS NOT NULL`, Example:
    sqlite> create table test(testid string);
    sqlite> .schema
    CREATE TABLE test(testid string);
    sqlite> select 1 where exists(select * from test where testid is null);
    sqlite> select 1 where exists(select * from test where testid is not null);
    sqlite> select 1 where not exists(select * from test where testid is null);
    1
    sqlite> select 1 where not exists(select * from test where testid is not null);
    1
    sqlite> insert into test(testid) values('foo');
    sqlite> insert into test(testid) values(NULL);
    sqlite> select * from test;
    foo

    sqlite> select 1 where exists(select * from test where testid is null);
    1
    sqlite> select 1 where exists(select * from test where testid is not null);
    1
    sqlite> select 1 where not exists(select * from test where testid is null);
    sqlite> select 1 where exists(select * from test where testid is not null);
    1

# Known Bugs

## Tricky Aggregates may have logic bug

Replicate by running the agent test-suite.