# Data Connector Agent for SQLite

This directory contains an SQLite implementation of a data connector agent.
It can use local SQLite database files as referenced by the "db" config field.

## Capabilities

The SQLite agent currently supports the following capabilities:

* [x] GraphQL Schema
* [x] GraphQL Queries
* [x] Relationships
* [x] Aggregations
* [x] Prometheus Metrics
* [x] Exposing Foreign-Key Information
* [x] Mutations
* [x] Native (Interpolated) Queries
* [ ] Subscriptions
* [ ] Streaming Subscriptions

Note: You are able to get detailed metadata about the agent's capabilities by
`GET`ting the `/capabilities` endpoint of the running agent.

## Requirements

* NodeJS 16
* SQLite `>= 3.38.0` or compiled in JSON support
    * Required for the json_group_array() and json_group_object() aggregate SQL functions
    * https://www.sqlite.org/json1.html#jgrouparray
* Note: NPM is used for the [TS Types for the DC-API protocol](https://www.npmjs.com/package/@hasura/dc-api-types)

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

## Docker Build & Run

```
> docker build . -t dc-sqlite-agent:latest
> docker run -it --rm -p 8100:8100 dc-sqlite-agent:latest
```

You will want to mount a volume with your database(s) so that they can be referenced in configuration.

## Options / Environment Variables

Note: Boolean flags `{FLAG}` can be provided as `1`, `true`, `t`, `yes`, `y`, or omitted and default to `false`.

| ENV Variable Name | Format | Default | Info |
| --- | --- | --- | --- |
| `PORT` | `INT` | `8100` | Port for agent to listen on. |
| `PERMISSIVE_CORS` | `{FLAG}` | `false` | Allows all requests - Useful for testing with SwaggerUI. Turn off on production. |
| `DB_CREATE` | `{FLAG}` | `false` | Allows new databases to be created. |
| `DB_READONLY` | `{FLAG}` | `false` | Makes databases readonly. |
| `DB_ALLOW_LIST` | `DB1[,DB2]*` | Any Allowed | Restrict what databases can be connected to. |
| `DB_PRIVATECACHE` | `{FLAG}` | Shared | Keep caches between connections private. |
| `DEBUGGING_TAGS` | `{FLAG}` | `false` | Outputs xml style tags in query comments for deugging purposes. |
| `PRETTY_PRINT_LOGS` | `{FLAG}` | `false` | Uses `pino-pretty` to pretty print request logs |
| `LOG_LEVEL` | `fatal` \| `error` \| `info` \| `debug` \| `trace` \| `silent` | `info` | The minimum log level to output |
| `METRICS` | `{FLAG}` | `false` | Enables a `/metrics` prometheus metrics endpoint.
| `QUERY_LENGTH_LIMIT` | `INT` | `Infinity` | Puts a limit on the length of generated SQL before execution. |
| `DATASETS` | `{FLAG}` | `false` | Enable dataset operations |
| `DATASET_DELETE` | `{FLAG}` | `false` | Enable `DELETE /datasets/:name` |
| `DATASET_TEMPLATES` | `DIRECTORY` | `./dataset_templates` | Directory to clone datasets from. |
| `DATASET_CLONES` | `DIRECTORY` | `./dataset_clones` | Directory to clone datasets to. |
| `MUTATIONS` | `{FLAG}` | `false` | Enable Mutation Support. |


## Agent usage

The agent is configured as per the configuration schema. The valid configuration properties are:

| Property | Type | Default |
| -------- | ---- | ------- |
| `db` | `string` | |
| `tables` | `string[]` | `null` |
| `include_sqlite_meta_tables` | `boolean` | `false` |
| `explicit_main_schema` | `boolean` | `false `

The only required property is `db` which specifies a local sqlite database to use.

The schema is exposed via introspection, but you can limit which tables are referenced by

* Explicitly enumerating them via the `tables` property, or
* Toggling the `include_sqlite_meta_tables` to include or exclude sqlite meta tables.

The `explicit_main_schema` field can be set to opt into exposing tables by their fully qualified names (ie `["main", "MyTable"]` instead of just `["MyTable"]`).

## Dataset

The dataset used for testing the reference agent is sourced from:

* https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sql

### Datasets

Datasets support is enabled via the ENV variables:

* `DATASETS`
* `DATASET_DELETE`
* `DATASET_TEMPLATES`
* `DATASET_CLONES`

Templates will be looked up at `${DATASET_TEMPLATES}/${template_name}.sqlite` or `${DATASET_TEMPLATES}/${template_name}.sql`. The `.sqlite` templates are just SQLite database files that will be copied as a clone. The `.sql` templates are SQL script files that will be run against a blank SQLite database in order to create a clone.

Clones will be copied to `${DATASET_CLONES}/${clone_name}.sqlite`.

## Testing Changes to the Agent

Ensure you run the agent with `DATASETS=1 DATASET_DELETE=1 MUTATIONS=1` in order to enable testing of mutations.

Then run:

```sh
cabal run dc-api:test:tests-dc-api -- test --agent-base-url http://localhost:8100 sandwich --tui
```

From the HGE repo.

## Known Issues
* Using "returning" in insert/update/delete mutations where you join across relationships that are affected by the insert/update/delete mutation itself may return inconsistent results. This is because of this issue with SQLite: https://sqlite.org/forum/forumpost/9470611066

## TODO

* [x] Prometheus metrics hosted at `/metrics`
* [x] Pull reference types from a package rather than checked-in files
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
* [x] `NOT EXISTS IS NULL` != `EXISTS IS NOT NULL`
* [x] Mutation support
