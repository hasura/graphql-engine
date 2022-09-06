# Hasura GraphQL Engine Changelog

:warning: This file is deprecated and contains changelog only for older releases. Please visit [this page](https://hasura.io/changelog) or [the github releases page](https://github.com/hasura/graphql-engine/releases) to view the changelog for latest releases :warning:

## v2.10.1

### Bug fixes and improvements

- server: fix long identifiers in insert with parameters (fix #8741)
- server: fix 'nulls: first' and 'nulls: last' parsing for Postgres
- server: accept `extensions_schema` while adding a PostgreSQL source for the graphql-engine to install database extensions in the specified schema
- server: accept a default extensions schema (`HASURA_GRAPHQL_METADATA_DATABASE_EXTENSIONS_SCHEMA`) for the metadata database where graphql-engine will install database extensions
- console: add support extensions_schema on postgres connect/edit DB form

## v2.11.0-beta.1

### Bug fixes and improvements

- server: accept `extensions_schema` while adding a PostgreSQL source for the graphql-engine to install database extensions in the specified schema
- server: accept a default extensions schema (`HASURA_GRAPHQL_METADATA_DATABASE_EXTENSIONS_SCHEMA`) for the metadata database where graphql-engine will install database extensions
- server: add warning log for missing admin secret
- server: fix querying relationships defined using multiple columns on BigQuery
- server: fix 'nulls: first' and 'nulls: last' parsing for Postgres
- console: add custom names for streaming subscriptions
- console: add support for table query and subscription root fields visibility permissions
- console: fix the BigQuery row-level restrictions on boolean columns
- console: add support extensions_schema on postgres connect/edit DB form
- console: add support for new database-to-remote schema metadata format in old table relationships UI

## v2.10.0

### Introducing Apollo Federation v1 support (experimental)

HGE can now be used as a subgraph in an Apollo federated GraphQL server.
You can read more about this feature in the [docs](https://hasura.io/docs/latest/data-federation/apollo-federation/).

This is an experimental feature (can be enabled by setting
`HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: apollo_federation`). This is supported
over all databases. To expose a table in an Apollo federated gateway, we need
to enable Apollo federation in its metadata. This can be done via the
`*_track_table` metadata API or via Modify Table page in console.

Enabling Apollo Federation on a table would add the `@key` directive in the GraphQL schema with
fields argument set to the primary key of the table (say `id`), i.e:
```graphql
type user @key(fields: "id") {
  id: Int!
  name: String
  ...
}
```

### Behaviour changes

- server: When providing a JSON path in a JWT claims map, you can now use
  double-quotes as well as single-quotes. Escaped characters in strings will now
  be honored appropriately, in the same way as JSON.

- server: In certain error messages, JSON paths will use double-quotes instead
  of single-quotes to represent field access.

  For example, instead of `$.args['$set']`, you will see `$.args["$set"]`.

- cli: Use 2-spaces indent for GraphQL content in metadata instead of tabs (#8469)

  Example:
  <table>
  <thead>
    <tr>
      <th>Old Behaviour<pre>metadata/query_collections.yml</pre></th>
      <th>New Behaviour<pre>metadata/query_collections.yml</pre></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><pre>
  - name: allowed-queries
    definition:
      queries:
      - name: getAlbums
        query: |
        	query getAlbums {
        		albums {
        			id
        			title
        		}
        	}
      </pre></td>
      <td><pre>
  - name: allowed-queries
    definition:
      queries:
      - name: getAlbums
        query: |
          query getAlbums {
            albums {
              id
              title
            }
          }
      </pre></td>
    </tr>
  </tbody>
  </table>

### Update multiple records for Postgres

We are introducing a new way to allow updating multiple records in the same
transaction for Postgres sources (#2768).

For example, the following query can be used to run the equivalent of two
`update_by_pk` in a single transaction:

```graphql
update_artist_many(
  updates: [
    { where: { id: { _eq: 1 } },
      _set: { name: "new name", description: "other" }
    }
    { where: { id: { _eq: 2 } },
      _set: { name: "new name" }
    }
    ]
) {
  affected_rows
  returning {
    name
  }
}
```

However, this feature allows arbitrary updates, even when they overlap:


```graphql
update_artist_many(
  updates: [
    { where: { id: { _lte: 3 } },
      _set: { name: "first", description: "other" }
    }
    { where: { id: { _eq: 2 } },
      _set: { name: "second" }
    }
    { where: { id: { _gt: 2 } },
      _set: { name: "third", description: "hello" }
    }
    { where: { id: { _eq: 1 } },
      _set: { name: "done" }
    }
    ]
) {
  affected_rows
  returning {
    id
    name
  }
}
```

Given the table looked like this before the query:

id | name | description
-- | ---- | -----------
 1 | one  | d1
 2 | two  | d2
 3 | three | d3
 4 | four | d4

After executing the query, the table will look like:

id | name | description
-- | ---- | -----------
 1 | done  | other
 2 | second  | other
 3 | third | hello
 4 | third | hello

The returned data will look like this:

```json
{
  "data": {
    "update_artist_many": [
      {
        "affected_rows": 3,
        "returning": [
          {
            "id": 1,
            "name": "first"
          },
          {
            "id": 2,
            "name": "first"
          },
          {
            "id": 3,
            "name": "first"
          }
        ]
      },
      {
        "affected_rows": 1,
        "returning": [
          {
            "id": 2,
            "name": "second"
          }
        ]
      },
      {
        "affected_rows": 2,
        "returning": [
          {
            "id": 3,
            "name": "third"
          },
          {
            "id": 4,
            "name": "third"
          }
        ]
      },
      {
        "affected_rows": 1,
        "returning": [
          {
            "id": 1,
            "name": "done"
          }
        ]
      }
    ]
  }
}
```

The way it works is:
- we allow arbitrary `where` clauses (just like in a regular `update`)
- we allow arbitrary `update`s (`_set`, `_inc`, etc., depending on the field
    type)
- we run each update in sequence, in a transaction (if one of them fails,
    everything is rolled back)
- we collect the return value of each query and return a list of results

Please submit any feedback you may have for this feature at https://github.com/hasura/graphql-engine/issues/2768.

### Error message syntax

We are slowly standardizing the format of error messages, especially with regards to the way values are quoted.

Any errors generated during the parsing of GraphQL or the construction of the schema might have changed the way they quote certain values. For example, GraphQL names are now always quoted with single quotes, leading to changes such as the following.

_Before:_

```
field "nonexistent_root_field" not found in type: 'query_root'
```

_After:_

```
field 'nonexistent_root_field' not found in type: 'query_root'
```

If you are depending on the specific text of an error message and/or parsing the message, you may need to update your code accordingly.

Further changes are forthcoming along similar lines.

### Bug fixes and improvements

- server: Kriti `basicFunctions` now available for REST Connectors and Webhook Transforms
- server: Fix bug where Hasura SQL trigger was not dropped when MS SQL Server source is dropped
- server: Delete event trigger related database SQL triggers from tables when they are untracked
- server: Use `root_field_namespace` as prefix for remote schema (fix #8438)
- server: Fix prefix/suffix behaviour for `graphql-default` naming convention (fix #8544)
- server: Fix namespace visibility during introspection (fix #8434)
- server: Create missing SQL triggers, if any, while reloading metadata and startup.
- server: Fix name/enum transformation bugs in `graphql-default` naming convention (fix #8640)
- server: Parameterize array variables in generated SQL for queries and subscriptions
- server: Make postgres-client-cert fields: `sslcert`, `sslkey` and `sslpassword` optional
- server: Add `*_update_source` API to update configuration of a connected database (See [docs](https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/source/))
- server: Changes to the Rest Endpoints OpenAPI specification:
    - The nullability of items in the output is now always correctly reported
    - Scalars other than UUID are more consistently inlined
    - Objects now have a title and, when available, the same description as in the GraphQL schema
- server: Bump Kriti package version to support optional variable lookup in string interpolation (fix #8574)
- server: Generate unique intermediate column names in PostgreSQL SQL queries to workaround PostgreSQL's identifier length limitation (fix #3796)
- console: Hide TimescaleDB internal schemas from data tab for connected TimescaleDB databases
- console: Support naming convention in source customization for PostgreSQL DBs
- console: Fix bug where "Analyze" button in the API explorer would stay in analyzing state after analyze failed
- console: Fix missing remote database relationship info for databases other than default on new table relationships page
- build: Changes to the `hasura/graphql-engine` Docker image:
  - Default graphql-engine docker images (`hasura/graphql-engine:<VERSION>`) now use an Ubuntu base instead of Debian.
  - Debian flavour of images (`hasura/graphql-engine:<VERSION>.debian`) are still published to Docker Hub.

## v2.10.0-beta.1

### Introducing Apollo Federation v1 support (experimental)

HGE can now be used as a subgraph in an Apollo federated GraphQL server.
You can read more about this feature in [the RFC](https://github.com/hasura/graphql-engine/blob/master/rfcs/apollo-federation.md).

This is an experimental feature (can be enabled by setting
`HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: apollo_federation`). This is supported
over all databases. To expose a table in an Apollo federated gateway, we need
to enable Apollo federation in its metadata. This can be done via the
`*_track_table` metadata API and console support will be added soon.

For example, given a table called `user` in a database which is not being
tracked by Hasura, we can run `*_track_table` to enable Apollo federation for
the table:

```
POST /v1/metadata HTTP/1.1
Content-Type: application/json
X-Hasura-Role: admin
```
``` json
{
  "type": "pg_track_table",
  "args": {
    "table": "user",
    "schema": "public",
    "apollo_federation_config": {
        "enable": "v1"
    }
  }
}
```
The above API call would add the `@key` directive in the GraphQL schema with
fields argument set to the primary key of the table (say `id`), i.e:
```graphql
type user @key(fields: "id") {
  id: Int!
  name: String
  ...
}
```

### Behaviour changes

- server: When providing a JSON path in a JWT claims map, you can now use
  double-quotes as well as single-quotes. Escaped characters in strings will now
  be honored appropriately, in the same way as JSON.

- server: In certain error messages, JSON paths will use double-quotes instead
  of single-quotes to represent field access.

  For example, instead of `$.args['$set']`, you will see `$.args["$set"]`.

- cli: Use 2-spaces indent for GraphQL content in metadata instead of tabs (#8469)

  Example:
  <table>
  <thead>
    <tr>
      <th>Old Behaviour<pre>metadata/query_collections.yml</pre></th>
      <th>New Behaviour<pre>metadata/query_collections.yml</pre></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><pre>
  - name: allowed-queries
    definition:
      queries:
      - name: getAlbums
        query: |
        	query getAlbums {
        		albums {
        			id
        			title
        		}
        	}
      </pre></td>
      <td><pre>
  - name: allowed-queries
    definition:
      queries:
      - name: getAlbums
        query: |
          query getAlbums {
            albums {
              id
              title
            }
          }
      </pre></td>
    </tr>
  </tbody>
  </table>

### Update multiple records for Postgres

We are introducing a new way to allow updating multiple records in the same
transaction for Postgres sources (#2768).

For example, the following query can be used to run the equivalent of two
`update_by_pk` in a single transaction:

```graphql
update_artist_many(
  updates: [
    { where: { id: { _eq: 1 } },
      _set: { name: "new name", description: "other" }
    }
    { where: { id: { _eq: 2 } },
      _set: { name: "new name" }
    }
    ]
) {
  affected_rows
  returning {
    name
  }
}
```

However, this feature allows arbitrary updates, even when they overlap:


```graphql
update_artist_many(
  updates: [
    { where: { id: { _lte: 3 } },
      _set: { name: "first", description: "other" }
    }
    { where: { id: { _eq: 2 } },
      _set: { name: "second" }
    }
    { where: { id: { _gt: 2 } },
      _set: { name: "third", description: "hello" }
    }
    { where: { id: { _eq: 1 } },
      _set: { name: "done" }
    }
    ]
) {
  affected_rows
  returning {
    id
    name
  }
}
```

Given the table looked like this before the query:

id | name | description
-- | ---- | -----------
 1 | one  | d1
 2 | two  | d2
 3 | three | d3
 4 | four | d4

After executing the query, the table will look like:

id | name | description
-- | ---- | -----------
 1 | done  | other
 2 | second  | other
 3 | third | hello
 4 | third | hello

The returned data will look like this:

```json
{
  "data": {
    "update_artist_many": [
      {
        "affected_rows": 3,
        "returning": [
          {
            "id": 1,
            "name": "first"
          },
          {
            "id": 2,
            "name": "first"
          },
          {
            "id": 3,
            "name": "first"
          }
        ]
      },
      {
        "affected_rows": 1,
        "returning": [
          {
            "id": 2,
            "name": "second"
          }
        ]
      },
      {
        "affected_rows": 2,
        "returning": [
          {
            "id": 3,
            "name": "third"
          },
          {
            "id": 4,
            "name": "third"
          }
        ]
      },
      {
        "affected_rows": 1,
        "returning": [
          {
            "id": 1,
            "name": "done"
          }
        ]
      }
    ]
  }
}
```

The way it works is:
- we allow arbitrary `where` clauses (just like in a regular `update`)
- we allow arbitrary `update`s (`_set`, `_inc`, etc., depending on the field
    type)
- we run each update in sequence, in a transaction (if one of them fails,
    everything is rolled back)
- we collect the return value of each query and return a list of results

Please submit any feedback you may have for this feature at https://github.com/hasura/graphql-engine/issues/2768.

### Bug fixes and improvements


- server: Kriti `basicFunctions` now available for REST Connectors and Webhook Transforms
- server: Fix bug where Hasura SQL trigger was not dropped when MS SQL Server source is dropped
- server: Delete event trigger related database SQL triggers from tables when they are untracked
- server: Use `root_field_namespace` as prefix for remote schema (fix #8438)
- server: Fix prefix/suffix behaviour for `graphql-default` naming convention (fix #8544)
- server: Fix namespace visibility during introspection (fix #8434)
- server: Create missing SQL triggers, if any, while reloading metadata and startup.
- server: Fix name/enum transformation bugs in `graphql-default` naming convention (fix #8640)
- server: Parameterize array variables in generated SQL for queries and subscriptions
- server: Make postgres-client-cert fields: `sslcert`, `sslkey` and `sslpassword` optional
- server: Add `*_update_source` API to update configuration of a connected database (See [docs](https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/source/))
- server: Changes to the Rest Endpoints OpenAPI specification:
    - The nullability of items in the output is now always correctly reported
    - Scalars other than UUID are more consistently inlined
    - Objects now have a title and, when available, the same description as in the GraphQL schema
- server: Bump Kriti package version to support optional variable lookup in string interpolation (fix #8574)
- server: Generate unique intermediate column names in PostgreSQL SQL queries to workaround PostgreSQL's identifier length limitation (fix #3796)
- console: Hide TimescaleDB internal schemas from data tab for connected TimescaleDB databases
- console: Support naming convention in source customization for PostgreSQL DBs
- console: Fix bug where "Analyze" button in the API explorer would stay in analyzing state after analyze failed
- console: Fix missing remote database relationship info for databases other than default on new table relationships page
- build: Changes to the `hasura/graphql-engine` Docker image:
  - Default graphql-engine docker images (`hasura/graphql-engine:<VERSION>`) now use an Ubuntu base instead of Debian.
  - Debian flavour of images (`hasura/graphql-engine:<VERSION>.debian`) are still published to Docker Hub.
  - CentOS flavour of images (`hasura/graphql-engine:<VERSION>.centos`) are no longer supported.
- docs: Kriti templating documentation sections added
  
## v2.9.0

### Event Triggers for MS SQL Server

(closes https://github.com/hasura/graphql-engine/issues/7228)

Event Triggers support has been added for MS SQL Server. Now, you can invoke external webhooks on insert/update/delete events on your MS SQL Server tables. See the [docs](https://hasura.io/docs/latest/graphql/core/event-triggers/index/) for more details.

### Bug fixes and improvements

- server: Support limit in BigQuery computed fields (fix #8562)
- server: Improve GraphQL query parsing time and per-query memory allocation
- server: Fix dropping column from a table that has update permissions (fix #8415)
- server: Fix `unsupported/unknown datatype was returned` error thrown when using `mssql_redeliver_event` API
- server: Fix bug with MS SQL Server events and shutdown handler
- server: Fix bug where Hasura SQL trigger was not dropped when an MS SQL Server database is dropped
- server: Allow all argument types for BigQuery routines
- console: Add support for computed fields with session arg in permission builder (fix #8321)
- console: Add GraphQL field customization for new database connections (root fields namespace, prefix, and suffix, and type names prefix and suffix)
- console: Fix notifications not being shown in certain cases on the console on Hasura Cloud
- console: Allow schemas prefixed with `pg`, but not `pg_` (fix #8435)
- console: Introduce new table relationships UI in alpha
- cli: Fix error reporting in `metadata apply` command (#8280)

## v2.9.0-beta.3

### Bug fixes and improvements

- server: Fix bug where Hasura SQL trigger was not dropped when an MS SQL Server database is dropped
- server: Allow all argument types for BigQuery routines
- console: Fix notifications not being shown in certain cases on the console on Hasura Cloud

## v2.9.0-beta.2

### Bug fixes and improvements

- server: fix dropping column from a table that has update permissions (fix #8415)
- server: fix `unsupported/unknown datatype was returned` error thrown when using `mssql_redeliver_event` API
- server: fix bug with MSSQL events and shutdown handler

## v2.8.4

### Bug fixes and improvements

- server: Add support to customize the root field for streaming subscriptions (fixes #8618)

## v2.8.3

### Bug fixes and improvements

- cli: fix performance regression with large metadata in `metadata apply`

  During the execution of `metadata apply` command, the YAML metadata is
  converted into JSON format because the server API accepts metadata in JSON
  format. For large metadata(> ~20k LOC), due to a recent change this conversion was
  taking upwards of 2 minutes of time, increasing exponentially with metadata size.
  With the changes in this release, the performance regression has been fixed.
  Following is a benchmark comparison of time taken for YAML to JSON conversion
  before and after the changes for different metadata sizes:
  | Metadata size(LOC) | Before(seconds) | After(seconds) |
  |--------------------|-----------------|----------------|
  |       10k          |      8.7        |     0.22       |
  |       20k          |     15.9        |     0.29       |
  |       50k          |     89.5        |     0.52       |
  |      100k          |    271.9        |     0.81       |
  |      300k          |      -          |     2.3        |


## v2.8.2

### Bug fixes and improvements

- server: revert allow casting most postgres scalar types to strings in comparison expressions (#8617)

## v2.9.0-beta.1

### Event Triggers for MS SQL Server

(closes https://github.com/hasura/graphql-engine/issues/7228)
Event Triggers support has been added for MS SQL Server. Now, you can invoke external webhooks on insert/update/delete events on your MS SQL Server tables. See the [docs](https://hasura.io/docs/latest/graphql/core/event-triggers/index/) for more details.

### Bug fixes and improvements

- server: add `*_update_source` API and modify behaviour of `*_add_source` API (See [docs](https://hasura.io/docs/latest/graphql/core/api-reference/metadata-api/source/) )
- server: support limit in BigQuery computed fields (fix #8562)
- server: improve GraphQL query parsing time and per-query memory allocation
- server: parameterize array variables in queries and subscriptions
- console: allow schemas prefixed with `pg`, but not `pg_` (fix #8435)
- console: add support for computed fields with session arg in permission builder (fix #8321)
- console: add GraphQL field customization for new database connections (root fields namespace, prefix, and suffix, and type names prefix and suffix)
- console: introduce new table relationships UI in alpha
- cli: fix performance regression with large metadata in `metadata apply`
- cli: fix error reporting in `metadata apply` command (#8280)
- server: query runtime performance optimizations
- server: fix bug that had disabled expression-based indexes in Postgress variants (fix Zendesk 5146)
- server: add optionality to additional postgres-client-cert fields: sslcert, sslkey and sslpassword

## v2.8.1

### Bug fixes and improvements

- server: fix bug that had disabled expression-based indexes in Postgres variants (fix #8601)

## v2.8.0

### Disabling query/subscription root fields

When a table is tracked in graphql-engine, three root fields are generated automatically
namely `<table>`, `<table>_by_pk` and `<table>_aggregate` in the `query` and the `subscription`
root. You can now control which root fields are exposed for a given role by specifying them in the select permission.

The main use-case for this feature is to disable APIs that access the table directly but which still need to be tracked so that:

1. It can be accessed via a relationship to another table
2. It can be used in select permissions for another table via a relationship

For such use-cases, we can disable all the root fields of the given table. This can be done by setting the select permission as follows:

```json
{
   "role": "user",
   "permission": {
     "columns": [
       "id",
       "name"
     ],
     "filter": {},
     "allow_aggregations": true,
     "query_root_fields": [],
     "subscription_root_fields": []
   }
 }
```

Another use-case is to allow a role to directly access a table only
if it has access to the primary key value. This can be done by setting the select permission as follows:

```json
{
   "role": "user",
   "permission": {
     "columns": [
       "id",
       "name"
     ],
     "filter": {},
     "allow_aggregations": false,
     "query_root_fields": ["select_by_pk"],
     "subscription_root_fields": ["select_by_pk"]
   }
 }
```

Note that console support for this permission will be released later.


### Introducing naming conventions (experimental)

Now, users can specify the naming convention of the auto-generated names in the HGE.
This is an experimental feature (enabled by setting `HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: naming_convention`) and is supported for postgres databases only for now. There are two naming
conventions possible:
| Naming Convention | Field names | Type names  | Arguments  | Enum values |
|-------------------|-------------|-------------|------------|-------------|
| `hasura-default`  | Snake case  | Snake case  | Snake case | as defined  |
| `graphql-default` | Camel case  | Pascal case | Camel case | Uppercased  |

Suppose there is a table called `my_table` and it has columns `id`, `date_of_birth`, `last_seen`, then with
`graphql-default` naming convention we will get the following auto-generated API:

```
query {
  myTable(orderBy: {dateOfBirth: asc}, limit: 10) {
    id
    dateOfBirth
    lastSeen
  }
}
```


To configure the naming convention for a source, set the naming convention in source
customisation while adding the source:

```JSON
{
  "resource_version": 2,
  "metadata": {
    "version": 1,
    "sources": [
      {
        "name": "default",
        "kind": "postgres",
        "tables": [],
        "configuration": {},
        "customization": {
          "naming_convention": "graphql-default"
        }
      }
    ]
  }
}
```

To set the default naming convention globally,
use the environment variable `HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION`.  Note
that the global default can be overridden by the source customisation setting mentioned above.

Note: Custom field names and custom table names will override the naming convention
(i.e. if the custom table name is `my_table` and `naming_convention`
is `graphql-default`, the field names generated will be `my_table`, `my_tableByPk`,
`my_tableAggregate` and so on).

### Behaviour Changes

- cli: change the ordering used for object fields in metadata files to alphabetical order

  Example:
  <table>
     <thead>
        <tr>
           <th>Server Metadata (JSON)</th>
           <th>Old behaviour (YAML)</th>
           <th>New Behaviour (YAML)</th>
        </tr>
     </thead>
     <tbody>
        <tr>
           <td>
              <pre>
  {
    "function": {
      "schema": "public",
      "name": "search_albums"
    }
  }
         </pre>
           </td>
           <td>
              <pre>
  function:
    schema: public
    name: search_albums
        </pre>
           </td>
           <td>
              <pre>
  function:
    name: search_albums
    schema: public
        </pre>
           </td>
        </tr>
     </tbody>
  </table>

### Bug fixes and improvements

- server: fix create event trigger failure for MSSQL sources on a table with a table name that is a reserved MSSQL keyword.
- server: errors from `/healthz` endpoint are now logged with more details
- server: do not expand environment variable references in logs or API responses from remote schemas, actions and event triggers for security reasons
- server: introduce [backend_only permissions](https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules/#backend-only-permissions) for update and delete mutations (fix #5275)
- server: add support for scalar array response type in actions
- server: add support for table computed fields in bigquery backends
- server: fix failure when executing consecutive delete mutations on mssql (#8462)
- server: bugfix: insertion of multiple empty objects should result in multiple entries (#8475)
- server: allow schemas prefixed with `pg`, but not `pg_` (fix hasura/graphql-engine#8435)
- console: add support for application/x-www-form-urlencoded in rest connectors (#8097)
- server: restore the ability to do no-op upserts (#8260).

## v2.8.0-beta.1

### Disabling query/subscription root fields

When a table is tracked in graphql-engine, three root fields are generated automatically
namely `<table>`, `<table>_by_pk` and `<table>_aggregate` in the `query` and the `subscription`
root. You can now control which root fields are exposed for a given role by specifying them in the select permission.

The main use-case for this feature is to disable APIs that access the table directly but which still need to be tracked so that:

1. It can be accessed via a relationship to another table
2. It can be used in select permissions for another table via a relationship

For such use-cases, we can disable all the root fields of the given table. This can be done by setting the select permission as follows:

```json
{
   "role": "user",
   "permission": {
     "columns": [
       "id",
       "name"
     ],
     "filter": {},
     "allow_aggregations": true,
     "query_root_fields": [],
     "subscription_root_fields": []
   }
 }
```

Another use-case is to allow a role to directly access a table only
if it has access to the primary key value. This can be done by setting the select permission as follows:

```json
{
   "role": "user",
   "permission": {
     "columns": [
       "id",
       "name"
     ],
     "filter": {},
     "allow_aggregations": false,
     "query_root_fields": ["select_by_pk"],
     "subscription_root_fields": ["select_by_pk"]
   }
 }
```

Note that console support for this permission will be released later.


### Introducing naming conventions (experimental)

Now, users can specify the naming convention of the auto-generated names in the HGE.
This is an experimental feature and is supported for postgres databases only for now. There are two naming
conventions possible:
| Naming Convention | Field names | Type names  | Arguments  | Enum values |
|-------------------|-------------|-------------|------------|-------------|
| `hasura-default`  | Snake case  | Snake case  | Snake case | as defined  |
| `graphql-default` | Camel case  | Pascal case | Camel case | Uppercased  |

Suppose there is a table called `my_table` and it has columns `id`, `date_of_birth`, `last_seen`, then with
`graphql-default` naming convention we will get the following auto-generated API:

```
query {
  myTable(orderBy: {dateOfBirth: asc}, limit: 10) {
    id
    dateOfBirth
    lastSeen
  }
}
```


To configure the naming convention for a source, set the naming convention in source
customisation while adding the source:

```JSON
{
  "resource_version": 2,
  "metadata": {
    "version": 1,
    "sources": [
      {
        "name": "default",
        "kind": "postgres",
        "tables": [],
        "configuration": {},
        "customization": {
          "naming_convention": "graphql-default"
        }
      }
    ]
  }
}
```

To set the default naming convention globally,
use the environment variable `HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION`.  Note
that the global default can be overridden by the source customisation setting mentioned above.

Note: Custom field names and custom table names will override the naming convention
(i.e. if the custom table name is `my_table` and `naming_convention`
is `graphql-default`, the field names generated will be `my_table`, `my_tableByPk`,
`my_tableAggregate` and so on).

### Behaviour Changes

- cli: change the ordering used for object fields in metadata files to alphabetical order

  Example:
  <table>
     <thead>
        <tr>
           <th>Server Metadata (JSON)</th>
           <th>Old behaviour (YAML)</th>
           <th>New Behaviour (YAML)</th>
        </tr>
     </thead>
     <tbody>
        <tr>
           <td>
              <pre>
  {
    "function": {
      "schema": "public",
      "name": "search_albums"
    }
  }
         </pre>
           </td>
           <td>
              <pre>
  function:
    schema: public
    name: search_albums
        </pre>
           </td>
           <td>
              <pre>
  function:
    name: search_albums
    schema: public
        </pre>
           </td>
        </tr>
     </tbody>
  </table>

### Bug fixes and improvements

- server: errors from `/healthz` endpoint are now logged with more details
- server: do not expand environment variable references in logs or API responses from remote schemas, actions and event triggers for security reasons
- server: introduce [backend_only permissions](https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules/#backend-only-permissions) for update and delete mutations (fix #5275)
- server: add support for scalar array response type in actions
- server: add support for table computed fields in bigquery backends
- server: fix failure when executing consecutive delete mutations on mssql (#8462)
- server: bugfix: insertion of multiple empty objects should result in multiple entries (#8475)
- server: allow schemas prefixed with `pg`, but not `pg_` (fix hasura/graphql-engine#8435)
- console: add support for application/x-www-form-urlencoded in rest connectors (#8097)
- server: restore the ability to do no-op upserts (#8260).

## v2.7.0

### Streaming subscriptions

Streaming subscriptions can be used to subscribe only to the data which has been changed in the
query. The streaming is done on the basis of a cursor, which is chosen by the user.
See [docs](https://hasura.io/docs/latest/graphql/core/databases/postgres/subscriptions/streaming/index/).

Request payload:

```graphql
subscription GetUserLatestMessages ($user_id: uuid!) {
  messages_stream (
    cursor: {
      initial_value: {id: 0},
      ordering: ASC
    },
    batch_size: 1,
    where: {user_id: {_eq: $user_id}}
  ) {
    id
    message
  }
}
```

The above subscription streams the messages of the user corresponding to the ``user_id`` in batches of 1 message per batch.

Suppose there are two messages to be streamed, then the server will send two responses as following:

Response 1:

```json
{
  "data": [
    {
      "id": 1,
      "message": "How are you!"
    }
  ]
}
```

Response 2:

```json
{
  "data": [
    {
      "id": 5,
      "message": "I am fine"
    }
  ]
}
```

### Bug fixes and improvements

- server: add support for custom scalar in action output type (#4185)
- server: add support for MSSQL event triggers (#7228)
- server: update pg_dump to be compatible with postgres 14 (#7676)
- server: fix bug where timestamp values sent to postgres would erroneously trim leading zeroes (#8096)
- server: fix metadata handling bug when event triggers were defined on tables that contained non lower-case alphabet characters (#8454)
- server: avoid encoding 'varchar' values to UTF8 in MSSQL backends
- server: fix dropping of nested typed null fields in actions (#8237)
- server: fix url/query date variable parsing bug in REST endpoints
- server: make url/query variables in REST endpoints assume string if other types not applicable
- server: fix inserting empty objects with default values to postgres, citus, and sql server (#8475)
- server: don't drop the SQL triggers defined by the graphql-engine when DDL changes are made using the `run_sql` API
- console: new "add remote schema" page with GQL customization
- console: allow users to remove prefix / suffix / root field namespace from a remote schema
- console: fix console crash on adding pg sources with connection params through api (#8416)
- cli: avoid exporting hasura-specific schemas during hasura init (#8352)
- cli: fix performance regression in `migrate status` command (#8398)

## v2.6.2

### Bug fixes and improvements

- server: fix inserting empty objects with default values to postgres, citus, and sql server (fix #8475)

## v2.7.0-beta.1

### Bug fixes and improvements

- server: don't drop the SQL triggers defined by the graphql-engine when DDL changes are made using the `run_sql` API
- server: fixed a bug where timestamp values sent to postgres would erroneously trim leading zeroes (#8096)
- server: fix bug when event triggers where defined on tables that contained non lower-case alphabet characters
- server: avoid encoding 'varchar' values to UTF8 in MSSQL backends
- server: add support for MSSQL event triggers (#7228)
- server: update pg_dump to be compatible with postgres 14 (#7676)
- server: fix parsing remote relationship json definition from 1.x server catalog on migration (fix #7906)
- server: Don't drop nested typed null fields in actions (fix #8237)
- server: fixes remote relationships on actions (fix #8399)
- server: fixes url/query date variable bug in REST endpoints
- server: makes url/query variables in REST endpoints assume string if other types not applicable
- server: fix inserting empty objects with default values to postgres, citus, and sql server (fix #8475)
- server: allow casting most postgres scalar types to strings in comparison expressions (fix #8524)
- console: add remote database relationships for views
- console: bug fixes for RS-to-RS relationships
- console: exclude `_timescaledb_internal` from db introspection sql, for performance reasons ()
- console: allow users to remove prefix / suffix / root field namespace from a remote schema
- console: new "add remote schema" page (with GQL customization)
- console: fix console crash on adding pg sources with connection params through api
- cli: avoid exporting hasura-specific schemas during hasura init (#8352)
- cli: fix performance regression in `migrate status` command (fix #8398)

## v2.6.1

- server: fix bug when event triggers where defined on tables that contained non lower-case alphabet characters (fix #8454)
- server: refactor insert mutations IR use of "default values" (fixes #8443)

## v2.5.2

- server: refactor insert mutations IR use of "default values" (fixes #8443)

## Milestone Release - v2.6.0

### Known issue
SQL Server: Mutation: [Default values overwritten on insert under certain conditions](https://github.com/hasura/graphql-engine/issues/8443). Will be addressed in 2.5.2 and 2.6.1.

### Announcing GraphQL Joins
We are delighted to announce Hasura’s data federation capabilities that accelerate the API development process by creating a single GraphQL schema from multiple data sources such as databases and remote GraphQL APIs. This allows you to query and mutate across federated data sources in real-time without writing any custom code. In addition, we can leverage many of Hasura’s powerful features from Hasura CE and Hasura Cloud.
Hasura’s field level permissions for remote schemas applies for joins as well, allowing for tightly controlled data disclosure when federating sources.
Leverage Hasura Cloud’s caching directive to instantly put caching in front of multiple remote GraphQL schemas.

### Breaking changes
The query and raw-query field from http-logs for metadata requests are removed by default. Use HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING to re-enable those fields.

### Bug fixes and improvements
- server: removed 'query' and 'raw-query' field from logs for metadata queries by default. Use HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING to re enable those fields.
- server: clear_metadata now correctly archives event triggers and the drop source API drops indirect dependencies created by remote relationships when the dependent source is dropped.
- server: fix inserting values into columns with case sensitive enum types for PostgreSQL/Citus backends (fix #4014)
- server: remote relationships can be defined on and to Microsoft SQL Server tables
- server: fix issues with JSON key encoding for remote schemas (fixes #7543 and #8200)
- server: fix Microsoft SQL Server insert mutation when relationships are used in check permissions (fix #8225)
- server: refactor GraphQL query static analysis and improve OpenAPI warning messages
- server: avoid a resource leak when connecting to PostgreSQL fails
- server: fix parsing remote relationship json definition from 1.x server catalog on migration (fix #7906)
- server: Don't drop nested typed null fields in actions (fix #8237)
- server: fixes remote relationships on actions (fix #8399)
- server: update pg_dump to be compatible with Postgres 14 (#7676)
- console: add support for setting aggregation query permissions for Microsoft SQL Server
- console: add support for RS-to-DB and RS-to-RS relationships to Remote Schemas
- console: removed the need for clicking the Modify btn before editing a remote schema (#1193, #8262)
- console: add remote database relationships support for views
- cli: fix remote schema metadata formatting issues (#7608)
- cli: fix query collections metadata formatting issues (#7616)
- cli: fix performance regression in migrate status command (fix #8398)
- docs: support for graphql-ws is considered GA

## v2.5.1

### Known issue
SQL Server: Mutation: [Default values overwritten on insert under certain conditions](https://github.com/hasura/graphql-engine/issues/8443). Will be addressed in 2.5.2 and 2.6.1.

### Bug fixes and improvements
- server: fixes remote relationships on actions (fix #8399)
- server: validate top level fragments in GQL query
- cli: fix performance regression in `migrate status` command (fix #8398)

## v2.6.0-beta.2

### Bug fixes and improvements
- server: fix parsing remote relationship json definition from 1.x server catalog on migration (fix #7906)
- server: Don't drop nested typed null fields in actions (fix #8237)
- server: fixes remote relationships on actions (fix #8399)
- server: update pg_dump to be compatible with postgres 14 (#7676)
- console: add remote database relationships for views
- cli: fix performance regression in `migrate status` command (fix #8398)

## v2.6.0-beta.1

### Breaking changes

- The `query` and `raw-query` field from http-logs for metadata requests are removed by default. Use
  `HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING` to renable those fields.

### Bug fixes and improvements

- server: Fix BigQuery overflow issue when using Decimal/BigDecimal data type.
- server: remove 'query' and 'raw-query' field from logs for metadata queries by default. Use `HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING` to renable those fields.
- server: `clear_metadata` now correctly archives event triggers and the drop source API drops indirect dependencies created by remote relationships when the dependent source is dropped.
- server: fix inserting values into columns with case sensitive enum types for PostgreSQL/Citus backends (fix #4014)
- server: remote relationships can be defined _on_ and _to_ Microsoft SQL Server tables
- server: fix issues with JSON key encoding for remote schemas (fixes #7543 and #8200)
- server: fix Microsoft SQL Server insert mutation when relationships are used in check permissions (fix #8225)
- server: refactor GraphQL query static analysis and improve OpenAPI warning messages
- server: avoid a resource leak when connecting to PostgreSQL fails
- console: add support for setting aggregation query permissions for Microsoft SQL Server
- console: add support for RS-to-DB and RS-to-RS joins to Remote Schemas tab
- console: removed the need for clicking the Modify btn before editing a remote schema (#1193, #8262)
- cli: fix remote schema metadata formatting issues (#7608)
- cli: fix query collections metadata formatting issues (#7616)
- docs: support for `graphql-ws` is considered GA

## v2.5.0

### Remote relationships from remote schemas

This release adds three new metadata API commands:
- `create_remote_schema_remote_relationship`
- `update_remote_schema_remote_relationship`
- `delete_remote_schema_remote_relationship`

that allows to create remote relationships between remote schemas on
the left-hand side and databases or remote schemas on the right-hand
side. Both use the same syntax as remote relationships from databases:

```yaml
type: create_remote_schema_remote_relationship
args:
  remote_schema: LeftHandSide
  type_name: LeftHandSideTypeName
  name: RelationshipName
  definition:
    to_remote_schema:
      remote_schema: RightHandSideSchema
      lhs_fields: [LHSJoinKeyName]
      remote_field:
        rhsFieldName:
          arguments:
            ids: $LHSJoinKeyName

type: create_remote_schema_remote_relationship
args:
  remote_schema: LeftHandSide
  type_name: LeftHandSideTypeName
  name: RelationshipName
  definition:
    to_source:
      source: RightHandSideSource
      table: {schema: public, name: RHSTable}
      relationship_type: object
      field_mapping:
        LHSJoinKeyName: RHSColumnName
```

Similarly to DB-to-DB relationships, only `Postgres` is supported on
the right-hand side for now.

### Deprecations
* The `custom_column_names` property of TableConfig used on `<db>_track_table` and `set_table_customization` metadata APIs has been deprecated in favour of the new `column_config` property. `custom_column_names` will still work for now, however, values used in `column_config` will take precedence over values from `custom_column_names` and any overlapped values in `custom_column_names` will be discarded.

### Behaviour Changes

- cli: use indentation of 2 spaces in array elements of metadata YAML files

  Example:
  <table>
  <thead>
    <tr>
      <th>Old behaviour<pre> metadata/query_collections.yaml</pre> </th>
      <th>New behaviour<pre> metadata/query_collections.yaml </pre> </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>
        <pre>
  - name: allowed-queries
    definition:
      queries:
      - name: getAlbums
        query: |
          query getAlbums {
            albums {
              id
              title
            }
          }
         </pre>
      </td>
      <td>
        <pre>
  - name: allowed-queries
    definition:
      queries:
        - name: getAlbums
          query: |
            query getAlbums {
              albums {
                id
                title
              }
            }
        </pre>
      </td>
    </tr>
  </tbody>
  </table>

  This change is a result of fixing some inconsistencies and edge cases in writing array elements.
  `hasura metadata export` will write YAML files in this format going forward. Also, note that this is a backwards compatible change.

- cli: change ordering of elements in metadata files to match server metadata

  Example:
  <table>
     <thead>
        <tr>
           <th>Server Metadata (JSON)</th>
           <th>Old behaviour (YAML)</th>
           <th>New Behaviour (YAML)</th>
        </tr>
     </thead>
     <tbody>
        <tr>
           <td>
              <pre>
  {
    "function": {
      "schema": "public",
      "name": "search_albums"
    }
  }
         </pre>
           </td>
           <td>
              <pre>
  function:
    name: search_albums
    schema: public
        </pre>
           </td>
           <td>
              <pre>
  function:
    schema: public
    name: search_albums
        </pre>
           </td>
        </tr>
     </tbody>
  </table>

### Streaming subscriptions

Streaming subscriptions can be used to subscribe only to the data which has been changed in the
query. The streaming is done on the basis of a cursor, which is chosen by the user.

Request payload:

```
subscription GetUserLatestMessages ($user_id: uuid!) {
  messages_stream (cursor: {initial_value: {id: 0}, ordering: ASC}, batch_size: 1, where: {user_id: {_eq: $user_id}} ) {
    id
    from
    to
  }
}
```

The above subscription streams the messages of the user corresponding to the ``user_id`` in batches of 1 message per batch.

Suppose there are two messages to be streamed, then the server will send two responses as following:

Response 1:

```
{
  "data": [
    {
      "id": 1,
      "from": 155234,
      "to": 155523
    }
    ]
}
```

Response 2:

```
{
  "data": [
    {
      "id": 5,
      "from": 178234,
      "to": 187523
    }
    ]
}
```

### Bug fixes and improvements

- server: improve error messages in BigQuery upstream API exceptions
- server: Fix regression in MSSQL subscriptions when results exceed 2048 characters (#8267)
- server: refactor OpenAPI spec generation (for REST endpoints) and improve OpenAPI warnings
- server: add jsonb to string cast support - postgres (#7818)
- server: improve performance of fetching postgres catalog metadata for tables and functions
- server: Queries present in query collections, such as allow-list and rest-endpoints, are now validated (fixes #7497)
- server: improve SQL generation for BigQuery backend queries involving `Orderby`.
- server: fix regression where remote relationships would get exposed over Relay, which is unsupported
- server: add support for customising the GraphQL schema descriptions of table columns in metadata
- server: column presets for SQL Server were broken and consequently insert and upsert mutations were failing with constraint violations. This change fixes this behavior (#8221).
- server: fix caching bug with session variables in remote joins
- server: fix regression where JWKs are refreshed once per second when both must-revalidate and max-age are specified in the Cache-Control header (#8299)
- server: respect custom field names in delete, insert and update mutations on SQL Server (#8314)
- console: enable searching tables within a schema in the sidebar
- console: add support for setting comments on the custom root fields of tables/views
- console: add feature flags section in settings
- console: improved support for setting comments on computed fields
- console: fix the ability to create updated_at and created_at in the modify page (#8239)
- console: fix an issue where editing both a column's name and its GraphQL field name at the same time caused an error
- console: fix redirect to metadata status page on inconsistent inherited role (#8343)
- console: fix malformed request with REST live preview section (#8316)
- console: fixed actions search to be case-insensitive
- cli: add support for customization field in sources metadata (#8292)
- cli: fix inherited roles metadata not being updated when dropping all roles (#7872)
- ci: ubuntu and centos flavoured graphql-engine images are now available

## v2.4.0

### Bug fixes and improvements

- server: add custom function for case insensitive lookup in session variable in request transformation
- server: add metadata inconsistency information in reload_metadata API call
- server: Webhook Transforms can now delete request/response bodies explicitly.
- server: Fix truncation of session variables with variable length column types in MSSQL (#8158)
- server: improve performance of `replace_metadata` for large schemas
- server: improve baseline memory consumption for typical workloads
- server: fix parsing timestamp values in BigQuery backends (fix #8076)
- server: add support for customising the GraphQL schema descriptions of table root fields
- server: add a `request_headers` field to the `test_webhook_transform` API.
- console: include cron trigger with include in metadata as false on cron trigger manage page
- console: show an error notification if Hasura CLI migrations fail
- console: fixed an issue where cron triggers were not removed from the list after deletion
- console: only show tables from current schema in clone permissions section
- console: provide checkbox to remove body in rest connectors
- cli: fix metadata version being set to 3 when doing `hasura init --version 2` (#8148)

## v2.4.0-beta.3

- server: fix regression in MSSQL subscriptions when results exceed 2048 characters (#8267)

## v2.4.0-beta.2

- server: fix regression where remote relationships would get exposed over Relay, which is unsupported

## v2.3.1

- server: fix regression where remote relationships would get exposed over Relay, which is unsupported

## v2.2.2

- server: fix regression where remote relationships would get exposed over Relay, which is unsupported

## v2.4.0-beta.1

### Bug fixes and improvements
(Add entries below in the order of server, console, cli, docs, others)

- server: add metadata inconsistency information in `reload_metadata` API call
- server: add custom function for case insensitive lookup in session variable in request transformation
- server: Improved error messaging for `test_webhook_transform` metadata API endpoint
- server: Webhook Tranforms can now produce `x-www-url-formencoded` bodies.
- server: Webhook Transforms can now delete request/response bodies explicitly.
- server: Fix truncation of session variables with variable length column types in MSSQL (#8158)
- server: improve performance of `replace_metadata` for large schemas
- server: improve baseline memory consumption for typical workloads
- server: fix parsing timestamp values in BigQuery backends (fix #8076)
- server: add support for customising the GraphQL schema descriptions of table root fields
- server: add a `request_headers` field to the `test_webhook_transform` API.
- server: add support for relationships on nested action fields
- console: include cron trigger with include in metadata as false on cron trigger manage page
- console: show an error notification if Hasura CLI migrations fail
- console: fixed an issue where cron triggers were not removed from the list after deletion
- console: only show tables from current schema in clone permissions section
- console: provide checkbox to remove body in rest connectors
- cli: fix metadata version being set to 3 when doing `hasura init --version 2` (#8148)

## v2.3.0

### Experimental SQL optimizations
Row-level permissions are applied by a translation into SQL `WHERE` clauses. If
some tables have similar row-level permission filters, then the generated SQL
may be repetitive and not perform well, especially for GraphQL queries that make
heavy use of relationships.

This version includes an _experimental_ optimization for some SQL queries.  It
is expressly experimental, because of the security-sensitive nature of the
transformation that it applies. You should scrutinize the optimized SQL
generated by this feature before using it in production.

The optimization can be enabled using the
`--experimental-features=optimize_permission_filters` flag or the
`HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` environment variable.

### Breaking changes
* Computed field comments are now used as the description for the field in the GraphQL schema. This means that computed fields where the comment has been set to empty string will cause the description of the field in the GraphQL schema to also be blank. Setting the computed field comment to null will restore the previous auto-generated description. The previous version of the Console would set the comment to empty string if the comment textbox was left blank, so some existing computed fields may unintentionally have empty string set as their comment.

### Bug fixes and improvements

- server: validate saved REST endpoint queries wrt schema
- server: improved error reporting for env vars in `test_webhook_transform` metadata API endpoint
- server: extend allowlist metadata with scope information, new command `update_scope_of_allowlist_in_metadata`
- server: (Postgres, Citus, and MSSQL backends) Identity columns and computed
  columns are now marked immutable, removing them from the schema of insert and
  update mutations.
- server: allow inserting more than 1 row simultaneously into table with generated columns (fix #4633)
  that have generated columns in Postgres.
- server: postgres: return a single entry per row (selected randomly) when an object relationship has multiple matches (fix #7936)
- server: Updates Kriti to v0.3.0
- server: add operation name in the request sent to remote schemas
- server: add support for scalar response types for actions (fix #7805)
- server: fix nullable action response (fix #4405)
- server: add support for customization of table & computed field GraphQL schema descriptions (fix #7496)
- server: classify MSSQL exceptions and improve API error responses
- server: MSSQL generates correct SQL when object relationships are null.
- console: add support for remote database relationships
- console: enable support for update permissions for mssql
- cli: skip tls verfication for all API requests when `insecure-skip-tls-verify` flag is set (fix #4926)
- server: fix issues working with read-only DBs by reverting the need for storing required SQL functions in a `hdb_lib` schema in the user's DB
- server: Fix experimental sql optimization read from `HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` or `--experimental-features`

## v2.3.0-beta.3

- server: Fix experimental sql optimization read from `HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` or `--experimental-features`

## v2.2.1

- server: postgres: return a single entry per row (selected randomly) when an object relationship has multiple matches (fix #7936)
- console: add support for remote database relationships

## v2.3.0-beta.2

- server: fix issues working with read-only DBs by reverting the need for storing required SQL functions in a `hdb_lib` schema in the user's DB

## v2.3.0-beta.1


### Experimental SQL optimizations
Row-level permissions are applied by a translation into SQL `WHERE` clauses. If
some tables have similar row-level permission filters, then the generated SQL
may be repetitive and not perform well, especially for GraphQL queries that make
heavy use of relationships.

This version includes an _experimental_ optimization for some SQL queries.  It
is expressly experimental, because of the security-sensitive nature of the
transformation that it applies. You should scrutinize the optimized SQL
generated by this feature before using it in production.

The optimization can be enabled using the
`--experimental-features=optimize_permission_filters` flag or the
`HASURA_GRAPHQL_EXPERIMENTAL_FEATURES` environment variable.

### Breaking changes
* Computed field comments are now used as the description for the field in the GraphQL schema. This means that computed fields where the comment has been set to empty string will cause the description of the field in the GraphQL schema to also be blank. Setting the computed field comment to null will restore the previous auto-generated description. The previous version of the Console would set the comment to empty string if the comment textbox was left blank, so some existing computed fields may unintentionally have empty string set as their comment.

### Bug fixes and improvements

- server: validate saved REST endpoint queries wrt schema
- server: improved error reporting for env vars in `test_webhook_transform` metadata API endpoint
- server: extend allowlist metadata with scope information, new command `update_scope_of_allowlist_in_metadata`
- server: (Postgres, Citus, and MSSQL backends) Identity columns and computed
  columns are now marked immutable, removing them from the schema of insert and
  update mutations.
- server: allow inserting more than 1 row simultaneously into table with generated columns (fix #4633)
  that have generated columns in Postgres.
- server: postgres: return a single entry per row (selected randomly) when an object relationship has multiple matches (fix #7936)
- server: Updates Kriti to v0.3.0
- server: add operation name in the request sent to remote schemas
- server: add support for scalar response types for actions (fix #7805)
- server: fix nullable action response (fix #4405)
- server: add support for customization of table & computed field GraphQL schema descriptions (fix #7496)
- server: classify MSSQL exceptions and improve API error responses
- server: MSSQL generates correct SQL when object relationships are null.
- console: add support for remote database relationships
- console: enable support for update permissions for mssql
- cli: skip tls verfication for all API requests when `insecure-skip-tls-verify` flag is set (fix #4926)

## v2.2.0

### Nested Action Types
Actions now support nested responses as described by associated action types.Example:
```graphql
type Product {
 id: bigint!
 name: String
}
type ElasticOutput {
 products: [Product!]!
 aggregations: jsonb
}
```
Previously, nested responses could be encapsulated in a generic "jsonb" output type but this loses precise type information for the API. The current support now allows specifying complex return types for the output.

Currently limits action relationships to top-level fields in the output types.

### GraphQL REST Endpoints OpenAPI Body Specifications

GraphQL REST endpoints are documented via Swagger (OpenAPI) under the `/api/swagger/json` endpoint. We now document the request and response bodies of the endpoints in addition to previous information.

### MS SQL Server Update for Hasura Server

##### Expand Transactions to GraphQL Queries and mssql_run-sql API

Extend transactions to GraphQL queries and mssql_run_sql API

##### Rollback a Transaction Based in the Transaction State

We can query the transaction state using XACT_STATE() scalar function. If the transaction is not active, don't rollback the transaction.

##### Upsert - SQL Generation and Execution

We are translating the if_matched section from the graphql, which is represented by the if_matched  type, to a MERGE SQL statement. Example:

```
mutation {
  insert_author(
    objects: { id: 1, name: "aaa" }
    if_matched: { match_columns: author_pkey, update_columns: name }
  ) {
    returning {
      id
      name
    }
  }
}
```

### Breaking Changes
- For any **MSSQL** backend, count aggregate query on multiple columns is restricted with a GraphQL
  schema change as follows

```diff
count (
---  columns: [table_select_column!]
+++  column: table_select_column
  distinct: Boolean
): Int!
```
  MSSQL doesn't support applying `COUNT()` on multiple columns.


### Bug fixes and improvements

- server: add a placeholder field to the schema when the `query_root` would be empty
- server: fix invalid GraphQL name in the schema arising from a remote relationship from a table in a custom schema
- server: add a new metadata API `get_cron_triggers` to fetch all the cron triggers
- server: add response transforms for actions, events, and triggers
- server: bigquery: implement `distinct_on`.
- server: extend transactions to MSSQL GraphQL queries and `mssql_run_sql` /v2/query API
- server: improve error messages in MSSQL database query exceptions
- server: in mssql transactions, rollback only if the transaction is active
- server: add request and response bodies to OpenAPI specification of REST endpoints
- server: implement upsert mutations for MS SQL Server (close #7864)
- server: extend support for insert mutations to tables without primary key constraint in a MSSQL backend
- server: fix parsing FLOAT64s in scientific notation and non-finite ones in BigQuery
- server: extend support for the `min`/`max` aggregates to all comparable types in BigQuery
- server: fix support for joins in aggregates nodes in BigQuery
- server: fix for passing input objects in query variables to remote schemas with type name customization (#7977)
- server: fix REST endpoints with path segments not showing correctly in the OpenAPI spec
- server: fix aliases used in GraphQL queries in REST endpoints not being reflected in the OpenAPI spec
- server: refresh JWKs a maximum of once per second (fix #5781)
- server: implement update mutations for MS SQL Server (closes #7834)
- server: support nested output object types in actions (#4796)
- server: action webhook requests now include a User-Agent header (fix #8070)
- console: action/event trigger transforms are now called REST connectors
- console: fix list of tables (and schemas) being unsorted when creating a new trigger event (fix #6391)
- console: fix custom field names breaking browse table sorting and the pre-populating of the edit row form
- console: enable support for insert & delete permissions for mssql tables
- console: enable inherited role on settings page
- cli: migrate and seed subcommands has an option in prompt to choose and apply operation on all available databases
- cli: fix `metadata diff --type json | unified-json` behaving incorrectly and showing diff in YAML format.
- cli: fix regression in `migrate create` command (#7971)
- cli: stop using `/healthz` endpoint to determine server health
- cli: fix regression with `--address` flag of `hasura console` command (#8005)

## v2.1.1

- server: provides a more comprehensive fix for the JSON ser/de backwards incompatibility that was initially addressed by 45481db (#7906)

## v2.1.0

- server: fix issue interpreting urls from environment in the `TestWebhookTransform` endpoint.
- server: fixes JSON ser/de backwards incompatibility introduced for metadata parsing and 'create_remote_relationship' queries (#7906)
- console: add sample context section to webhook transforms
- cli: `hasura metadata diff` shows diff with more context in directory mode
- cli: revert change to split metadata related to remote schemas into seperate files (introduced in v2.1.0-beta.2)

## v2.1.0-beta.3

- server: allows the use of mock env vars in the `test_webhook_transform` metadata API action
- server: fix event invocation logs to include transformed request bodies
- server: fix aggregate queries with nodes field in selection set for sql server (fix #7871)
- server: fix permissions are not respected for aggregations in sql server (fix #7773)
- server: the syntax for remote relationships in metadata is changed to be
  consistent with future remote relationships work. However, the older syntax
  is still accepted and this is a non-breaking change.
- server: implement delete mutations for MS SQL Server (closes #7626)
- server: fix JSON path in error when parsing sources in metadata (fix #7769)
- server: log locking DB queries during source catalog migration
- server: fix to allow remote schema response to contain an "extensions" field (#7143)
- server: support database-to-database joins with BigQuery
- server: improved startup time when using large remote schemas
- server: fix rest-endpoints bug allow list arguments (fix #7135)
- server: fallback to unauthorized role when JWT is not found in cookie (fix #7272)
- server: add support for building linux/arm64 docker image (#6337, #1266)
- server: provide option to explicitly recreate event triggers for sources in the `reload_metadata` API
- server: fix `gen_hasura_uuid` migration to be idempotent, so that it doesn't fail if the database is already initialised with source migrations.
- server: fix mssql `table_by_pk` query returning empty array (fix #7784)
- server: fix BigQuery queries failing with more than one array relationship
- console: add comments to tracked functions
- console: add select all columns option while selecting the columns in event triggers
- console: add request transforms for events
- metadata SDK: add type definitions for config v3
- cli: fix cli-console failing to add migrations if there are tabs in SQL body (#7362)
- cli: sign windows binary of Hasura CLI (#7147)
- cli: core CLI features are not blocked in environments without internet (#7695)
- server: add `_like`/`_nlike` and spatial operators for BigQuery

## v2.1.0-beta.2

### Action transforms

Action transforms are used to perform transformations on the HTTP request generated by an action.
This allows you to integrate REST APIs or existing APIs without writing a middleware service
that transforms the action's request to the one expected by the API.

Read more in the docs.

### Function field names customization (#7405)

It is now possible to specify the GraphQL names of tracked SQL functions in
Postgres sources, and different names may be given to the `_aggregate` and
suffix-less versions.  Aliases may be set by both
`/v1/metadata/pg_track_function` and the new API endpoint
`/v1/metadata/pg_set_function_customization.`

### Root field name and type name customization per source (#6974)

When adding a source it is now possible to specify prefixes and suffixes
that will be added to all root field names and type names generated for that
source. It is also possible to specify a root "namespace" field to use for the
source.

### Bug fixes and improvements

- server: do not recreate event triggers if tables haven't changed on reloading metadata
- server: moves `request_transform` into the Action Definition the `create_action` metadata API call.
- server: call auth webhooks even when the request is malformed JSON or otherwise fails to parse (close #7532)
- server: updates kriti to v0.2.1 which adds an `escapeUri` function
- server: add cascade option to `mssql_run_sql` metadata API
- server: fix bug which recreated event triggers every time the graphql-engine started up
- server: fix bug in OpenAPI when multiple REST endpoints have the same URL path but different method
- server: add support for GraphQL block strings
- server: Correctly translate permissions on functions to SQL (#7617)
- server: add transformed request to action error responses
- server: allow nullable action responses (#4405)
- server: add support for openapi json of REST Endpoints
- server: enable inherited roles by default in the graphql-engine
- server: support MSSQL insert mutations
- server: fix bug in OpenAPI when multiple REST endpoints have the same URL path but different method
- server: forward Set-Cookie headers from auth webhook
- console: design cleanup Modify and Add Table forms (close #7454)
- console: enable custom graphql root fields for mssql under modify tab
- console: allow dropping indices on all schemas
- console: fix bug with displaying 1-to-1 relationship with the same column mapping (close #7552)
- console: add request transforms for actions
- console: fix v2 metadata imports
- console: design cleanup Modify and Add Table forms (close #7454)
- console: enable custom graphql root fields for mssql under modify tab
- cli: split remote schema permissions metadata into seperate files (#7033)
- cli: support action request transforms in metadata
- cli: make `--database-name` optional in `migrate` subcommands when using a single database (#7434)
- cli: support absolute paths in --envfile (#5689)
- cli: split remote schema permissions metadata into seperate files (#7033)

## v2.0.10

- server: fix bug which recreated event triggers every time the graphql-engine started up
- server: remove identity notion for table columns (fix #7557)
- console: add performance fixes for handling large db schemas

## v2.1.0-beta.1

- server: Ignore unexpected fields in action responses (#5731)
- server: add webhook transformations for Actions and EventTriggers
- server: optimize SQL query generation with LIMITs
- server: add GraphQL request query in the payload for synchronous actions
- server: improve the event trigger logging on errors
  NOTE: This change introduces a breaking change, earlier when there
  was a client error when trying to process an event, then the status was reported as 1000. Now, the status 1000 has been removed and if any status was received by the graphql-engine from the webhook, the status
  of the invocation will be the same otherwise it will be `NULL`.
- server: support `extensions` field in error responses from action webhook endpoints (fix #4001)
- server: fix custom-check based permissions for MSSQL (#7429)
- server: query performance improvements
- server: remove identity notion for table columns (fix #7557)
- server: support MSSQL transactions
- server: log individual operation details in the http-log during a batch graphQL query execution
- server: update `create_scheduled_event` API to return `event_id` in response
- server: fix bug which allowed inconsistent metadata to exist after the `replace_metadata` API even though `allow_inconsistent_object` is set to `false`.
- server: fix explicit `null` values not allowed in nested object relationship inserts (#7484)
- server: `introspect_remote_schema` API now returns original remote schema instead of customized schema
- server: prevent empty subscription roots in the schema (#6898)
- server: support database-to-database joins (for now, limited to Postgres as the target side of the join)
- server: add support for user comments for trackable functions (#7490)
- console: support tracking of functions with return a single row
- console: add GraphQL customisation under Remote schema edit tab
- console: fix cross-schema array relationship suggestions
- console: add performance fixes for handle large db schemas
- console: fix missing cross-schema computed fields in permission builder
- console: add time limits setting to security settings
- cli: add support for `network` metadata object
- cli: `hasura migrate apply --all-databases` will return a non zero exit code if operation failed on atleast one database (#7499)
- cli: `migrate create --from-server` creates the migration and marks it as applied on the server
- cli: support `query_tags` in metadata
- cli: add `hasura deploy` command
- cli: allow exporting and applying metadata from `yaml/json` files
- cli: allow squashing specific set of migrations. A new `--to` flag is introduced in `migrate squash` command. eg: `hasura migrate squash --from <v1> --to <v4>`
- cli: `hasura init --endpoint <endpoint>` adds an option to export metadata and create initial migration from the server.

## v2.0.9

- server: fix export_metadata V2 bug which included cron triggers with `include_in_metadata: false`
- server: disable mutation for materialised views (#6688)
- server: set `tracecontext` and `userInfo` for DML actions on Postgres sources
- server: add support for `connection_parameters` on `pg_add_source` API
- cli: add progress bar for `migrate apply` command (#4795)
- cli: embed cli-ext for windows binaries (#7509)

## v2.0.8

- server: fix nullability of object relationships (close #7201)
- server: update non-existent event trigger, action and query collection error msgs (close #7396)
- server: fix broken `untrack_function` for non-default source
- server: Adding support for TLS allowlist by domain and service id (port)
- server: add support for `graphql-ws` clients
- console: fix error due to rendering inconsistent object's message
- console: support insecure TLS allowlist
- console: support computed fields in remote schema join
- console: fix data sidebar not updated when a table is renamed
- cli: fix delay starting console using `hasura console` (#7255)

## v2.0.7

- server: fix v2 -> v1 downgrade bug when cron triggers exist
- server: add index on the `event_id` column of the `hdb_cron_event_invocation_logs` table
- server: fix GraphQL type for remote relationship field (close #7284)
- server: support EdDSA algorithm and key type for JWT
- server: fix GraphQL type for single-row returning functions (close #7109)
- console: add support for creation of indexes for Postgres data sources
- docs: document the cleanup process for scheduled triggers
- console: allow same named queries and unnamed queries on allowlist file upload
- console: support computed fields in permission builder
- console: add custom timeouts to actions

## v2.0.6

- server: Add support for inherited roles for mutations, remote schema, actions and custom function permissions
- server: fix an issue with remote relationships when join columns are aliased (close #7180)
- server: fix for incorrect `__typename` value in nested remote joins with a customized remote schema
- server: fix a bug where some unicode characters in default string values for fields in remote schemas could lead to internal errors
- server: bigquery: implement `_in` and `_nin` operators. (close #7343)
- server: bigquery: custom root names, table names and field names for bigquery are included in tests
- console: fix untracked foreign-key relationships suggestion across schemas
- console: allow resolution of conflicting inherited role permissions
- cli: fix SDL formatting in `actions.graphql`(#7296)

## v2.0.5

- server: prevent invalid collisions in remote variable cache key (close #7170)
- server: preserve unchanged cron triggers in `replace_metadata` API
- server: fix inherited roles bug where mutations were not accessible when inherited roles was enabled
- server: reintroduce the unique name constraint in allowed lists
- server: subscriptions now validate that all session variables are properly set (#7111)
- console: fix metadata out-of-date errors when creating tables with certain configurations (fix #6805) (fix #7233)
- cli-migrations-v2: fix database url showing up in metadata (#7319)

## v2.0.4

- server: Support computed fields in permission check/filter (close #7102)
- server: support computed fields in query 'order_by' (close #7103)
- server: log warning if there are errors while executing clean up actions after "drop source" (previously it would throw an error)
- server: Fixed a bug where MSSQL and BigQuery would ignore environment variables set from the console
- server: Fixing bug in ReplaceMetadata parser - Moving from Alternative to committed-choice.
- server: Relax the unique operation name constraint when adding a query to a query collection
- server: officially deprecate query plan caching, which had already been disabled for a long time
- server/bigquery: Fix issues related to adding and querying from non-US datasets (closes [6937](https://github.com/hasura/graphql-engine/issues/6937)).
- console: add template gallery
- console: add pagination on the Raw SQL results page
- console: fix issues with replacing invalid graphql identifiers in table and column names
- console: show error message on inconsistent objects table
- server/mssql: Fix [graphql-engine#7130](https://github.com/hasura/graphql-engine/issues/7130) for `__typename` errors and more
generally, JSON-style aggregates.
- cli: add support for `query_tags` metadata object

## v2.0.3
(Add entries below in the order of server, console, cli, docs, others)

- server: inherited role improvements for select queries
    - an inherited role can now inherit from other inherited roles as well
    - explicit permissions for inherited roles can now be set which will override the inherited permission (if any)
- server: fix optional global_select_limit config for BigQuery
- console: support `global_select_limit` for bigquery sources
- cli: add `-o`/`--output` flag for `hasura metadata inconsistency list` command

## v2.0.2

- server: only if `query-log` is enabled the graphql query string is printed in `http-log` and `websocket-log`
- server: fix reloading inconsistent sources or remote schemas via `reload_metadata` API
- server: support scalar computed fields in remote joins (close #7101)
- server: Support computed fields in query filter (`where` argument) (close #7100)
- server: add a `$.detail.operation.request_mode` field to `http-log` which takes the values `"single"` or `"batched"` to log whether a GraphQL request was executed on its own or as part of a batch
- server: add `query` field to `http-log` and `websocket-log` in non-error cases
- server: Add global limit to BigQuery via the `global_select_limit` field in the connection configuration
- server: include action and event names in log output
- server: log all HTTP errors in remote schema calls as `remote-schema-error` with details
- server: For BigQuery, make `global_select_limit` configuration optional with a default value of
`1000`
- console: add `reload all databases` checkbox to the metadata settings page
- console: add schema sharing
- console: fix issue with changing table's column name and graphQL field name simultaneously
- console: fix adding/removing/updating database not getting added to `metadata/databases.yaml` in CLI mode
- console: fix migrations being generated for allowed queries and inherited roles and in CLI mode
- cli: add linux and darwin arm64 support

## v2.0.1

- server: fix resetting metadata catalog version to 43 while initializing postgres source with v1.0 catalog

## v2.0.0

NOTE: This only includes the diff between v2.0.0 and v2.0.0-beta.2

- server: make improvements in the `livequery-poller-log`
- server: Backends Citus, MSSQL, and BigQuery now all support the `set_table_customization` operation.
- server: Adds caching support for queries using remote schema permissions
- server: All Postgres boolean operators now support the null-collapsing behaviour described in [#704](https://github.com/hasura/graphql-engine/issues/704) and enabled via the `HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE` environment variable.
- server: add `update_remote_schema` metadata query
- console: add citus support
- console: add support for `update_remote_schema` API while modifying remote schemas
- console: hide postgres system schemas by default
- cli: `metadata diff` will now only show the differences in metadata. old behaviour is avialble behind a flag (`--type unified-common`) (#5487)
- cli: add citus support
- cli: allow `--skip-execution` to be used with `up` and `down` flags in `migrate apply`
- cli: allow deleting migration state from server using `--server` flag in `migrate delete` command

## v2.0.0-beta.2

### Bug fixes and improvements

- server: nodes aggregates and inherited roles support for SQL Server
- server: remote relationships (database to remote schema joins) are now supported on SQL Server and BigQuery
- server: BigQuery: switches to a single query generation from a dataloader approach. This should result in
  faster query responses.
- server: BigQuery: various bug fixes related to aggregations
- server: fix add source API wiping out source's metadata when replace_configuration is true
- server: add support for customization of field names and type names when adding a remote schema
- console: add foreign key CRUD functionality to ms sql server tables
- console: allow tracking of custom SQL functions having composite type (rowtype) input arguments
- console: allow input object presets in remote schema permissions
- console: add modify functionality on columns, primary keys & unique keys to MS SQL Server tables
- cli: add interactive prompt to get input when `--database-name` flag is missing
- cli: fix metadata export to avoid unnecessary empty lines in actions.graphql (#5338)
- cli: generate migrations for mssql databases in hasura console mode  (#7011)

## v2.0.0-beta.1

### Bug fixes and improvements

- server: fix regression with MSSQL execution (#6976)
- server: fix asymptotic performance of fetching from the event_log table
- console: add `pool_timeout`, `connection_lifetime` and `isolation_level` connection params to connect database form
- console: add check constraints and comments to MS SQL Server tables' read-only modify page
- console: add create table functionality for MS SQL Server
- console: update connect database form with SSL certificates
- console: add drop table functionality to MS SQL Server tables
- console: allow renaming data sources
- console: show error notification for table and cloumn names exceeding 63 characters and trim migration names exceeding 255 characters
- cli: fix version command using stderr as output stream (#6998)

## v2.0.0-alpha.11

### Breaking Changes

- In this release, the name of the computed field argument has changed from `<function_name>_args` to
  `<computed_field_name>_<table_name>_args` as the function name is internal detail for a computed field.
  This change also enables adding a root-level tracked function as a computed field which previously would have thrown input type conflict error.

### Bug fixes and improvements

- server: detect and apply metadata changes by `mssql_run_sql` API if required
- server: fix bug with creation of new cron events when cron trigger is imported via metadata
- server: log warning for deprecated environment variables.
- server: initialise `hdb_catalog` tables only when required, and only run the event loop for sources where it is required
- server: fix a bug where remote schema permissions would fail when used in conjunction with query variables (fix #6656)
- server: add `rename_source` metadata API (fix #6681)
- server: fix subscriptions with session argument in user-defined function (fix #6657)
- server: MSSQL: Support ORDER BY for text/ntext types.
- server: MSSQL: Support _lt, _eq, etc. for text/ntext types.
- server: MSSQL: Fix offset when there's no order by.
- server: MSSQL: Support booleans better.
- server: Include permission filter in the exists clause (fix #6931)
- server: add support for adding multi-column foreign key relationships
- server: fix a bug where `@skip` and `@include` were not allowed on the same field
- server: properly reject queries containing unknown or misplaced directives
- server: fix bigint overflow, incorrect geojson format in event trigger payload (fix #3697) (fix #2368)
- server: fix introspection output not being consistently ordered
- server: forward the x-request-id header when generated by graphql-engine (instead of being user-provided) (fix #6654)
- server: fix erroneous schema type for action output fields (fix #6631)
- server: introduce `--graceful-shutdown-timeout` server config which will wait for the in-flight scheduled and event trigger events and async actions to complete before shutting down
- server: fix a regression from V1 and allow string values for most Postgres column types
- server: sanitise event trigger and scheduled trigger logs to omit possibly sensitive request body and headers
- server: fix parsing of values for Postgres char columns (fix #6814)
- server: fix query execution of custom function containing a composite argument type
- server: fix a bug in query validation that would cause some queries using default variable values to be rejected (fix #6867)
- server: REST endpoint bugfix for UUID url params
- server: custom URI schemes are now supported in CORS config (fix #5818) (#5940)
- server: explaining/analyzing a query now works for mssql sources
- server: fix MSSQL multiplexed subscriptions (fix #6887)
- server: fix bug preventing tables with the same name in different sources
- server: include more detail in inconsistent metadata error messages (fix #6684)
- server: return useful error messages for missing env vars in metadata when `allow_inconsistent_metadata` is enabled (fix #6913)
- console: add union types to remote schema permissions
- console: read-only modify page for mssql
- console: filter out partitions from track table list and display partition info
- console: fixes an issue where no schemas are listed on an MSSQL source
- console: allow editing sources configuration
- console: show db version and source details in manage db page
- console: add one-to-one relationships support
- console: rearrange connect database form and add prepared statements
- cli: add `-o`/`--output` flag for `hasura metadata` `apply` & `export` subcommands
```
# export metadata and write to stdout
$ hasura metadata export -o json
```
- cli: add support for `graphql_schema_introspection` metadata object
- cli: fix applying migrations in a different environment after config v3 update (#6861)
- cli: fix bug caused by usage of space character in database name (#6852)
- cli: fix issues with generated filepaths in windows (#6813)
- cli: add warning for incompatible pro plugin version
- cli: add new sub command `delete` to `hasura migrate`
- cli: fix bug in migrate squash due to empty down file (#3897)
- cli: fix bug with metadata apply on some CI environments (#6987)

## v2.0.0-alpha.10

### Bug fixes and improvements

- server: fix MSSQL table metadata SQL, return empty array for empty rows (fix #1226)
- server: aggregation fields are now supported on mssql
- server: accept a new server config flag `--events-fetch-batch-size` to configure the number of rows being fetched from the events log table in a single batch
- server: restore proper batching behavior in event trigger processing so that at most 2x batch events are checked out at a time
- server: fix regression: `on_conflict` was missing in the schema for inserts in tables where the current user has no columns listed in their update permissions (fix #6804)
- server: fix one-to-one relationship bug which prevented adding one-to-one relationships which didn't have the same column name for target and source
- console: fix Postgres table creation when table has a non-lowercase name and a comment (#6760)
- cli: fix regression - `metadata apply —dry-run` was overwriting local metadata files with metadata on server when it should just display the differences.
- server: decrease polling interval for scheduled triggers from 60 to 10 seconds
- server: Change `HASURA_GRAPHQL_SCHEMA_POLL_INTERVAL` env var to `HASURA_GRAPHQL_SCHEMA_SYNC_POLL_INTERVAL` and `schema-poll-interval` option to `--schema-sync-poll-interval`.

## v2.0.0-alpha.9

### Support comparing columns across related tables in permission's boolean expressions

We now support comparing columns across related tables. For example:

Consider two tables, `items(id, name, quantity)` and `shopping_cart(id, item_id, quantity)`
and these two tables are related via the `item_id` column. Now, while defining insert permission
on the `shopping_cart` table, there can be a check to insert an item into the shopping cart
only when there are enough present in the items inventory.

### Bug fixes and improvements

- server: make the ``HASURA_GRAPHQL_PG_CONN_LIFETIME``, ``HASURA_GRAPHQL_POOL_TIMEOUT`` and ``HASURA_GRAPHQL_TX_ISOLATION`` configs source specific
- server: fix bug with catalog upgrade from alpha.7 (fix #6802)
- server: fix a bug in remote schema permissions that could result in an invalid GraphQL schema (fix #6029, #6703)
- server: support query multiplexing in MSSQL subscriptions
- server: an inherited role's limit will be the max limit of all the roles (#6671)
- console: add bigquery support (#1000)
- cli: add support for bigquery in metadata operations

## v2.0.0-alpha.8

### Support for 3D PostGIS Operators

We now support the use of the functions `ST_3DDWithin` and `ST_3DIntersects` in boolean expressions.
Note that `ST_3DIntersects` requires PostGIS be [built with SFCGAL support](https://www.postgis.net/docs/manual-3.1/reference.html#reference_sfcgal) which may depend on the PostGIS distribution used.

### Support for null values in boolean expressions

In v2, we introduced a breaking change, that aimed at fixing a [long-standing issue](https://github.com/hasura/graphql-engine/issues/704): a null value in a boolean expression would always evaluate to `True` for all rows. For example, the following queries were all equivalent:

```graphql
delete_users(where: {_id: {_eq: null}})  # field is null, which is as if it were omitted
delete_users(where: {_id: {}})           # object is empty, evaluates to True for all rows
delete_users(where: {})                  # object is empty, evaluates to True for all rows
delete_users()                           # delete all users
```

This behaviour was unintuitive, and could be an unpleasant surprise for users that expected the first query to mean "delete all users for whom the id column is null". Therefore in v2, we changed the implementation of boolean operators to reject null values, as we deemed it safer:


```graphql
delete_users(where: {_id: {_eq: null}})  # error: argument of _eq cannot be null
```

However, this change broke the workflows of [some of our users](https://github.com/hasura/graphql-engine/issues/6660) who were relying on this property of boolean operators. This was used, for instance, to _conditionally_ enable a test:

```graphql
query($isVerified: Boolean) {
  users(where: {_isVerified: {_eq: $isVerified}}) {
    name
  }
}
```

In the future, we will probably offer a way to explicitly choose which behaviour to use for each `where` clause; perhaps by introducing new and distinct operators that make it explicit that they will default to true if the value is null. In the meantime, this release provides a way to revert the engine to its previous behaviour: if the `HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE` environment variable is set to "true", null values in boolean expression will behave like they did in v1 for the following operators: `_is_null`, `_eq`, `_neq`, `_in`, `_nin`, `_gt`, `_lt`, `_gte`, `_lte`.

### Bug fixes and improvements

- server: all /query APIs now require admin privileges
- server: add a new `/dev/rts_stats` endpoint, enabled when hasura is started with '+RTS -T'
- server: re-enable a default HASURA_GRAPHQL_PG_CONN_LIFETIME of 10min
- server: support for bigquery datasets
- server: format the values of `injectEventContext` as hexadecimal string instead of integer (fix #6465)
- server: add "kind" field to query-log items. Kind can be "database", "action", "remote-schema", "cached" or "introspection".
- console: add custom_column_names to track_table request with replaced invalid characters (#992)
- console: add details button to the success notification to see inserted row
- console: add request preview for REST endpoints
- cli: fix errors being ignored during `metadata apply` in config v3 (fix #6784)

## v2.0.0-alpha.7

### Transactions for Postgres mutations

With v2 came the introduction of heterogeneous execution: in one query or mutation, you can target different sources: it is possible, for instance, in one mutation, to both insert a row in a table in a table on Postgres and another row in another table on MSSQL:

```graphql
mutation {
  // goes to Postgres
  insert_author_one(object: {name: "Simon Peyton Jones"}) {
    name
  }

  // goes to MSSQL
  insert_publication_one(object: {name: "Template meta-programming for Haskell"}) {
    name
  }
}
```

However, heterogeneous execution has a cost: we can no longer run mutations as a transaction, given that each part may target a different database. This is a regression compared to v1.

While we want to fix this by offering, in the future, an explicit API that allows our users to *choose* when a series of mutations are executed as a transaction, for now we are introducing the following optimisation: when all the fields in a mutation target the same Postgres source, we will run them as a transaction like we would have in v1.


### Bug fixes and improvements

- server: `use_prepared_statements` option (default: False) in `add_pg_source` metadata API
- server: add `--async-actions-fetch-interval` command-line flag and `HASURA_GRAPHQL_ASYNC_ACTIONS_FETCH_INTERVAL` environment variable for configuring
          async actions re-fetch interval from metadata storage (fix #6460)
- server: add 'replace_configuration' option (default: false) in the add source API payload
- server: add a comment field for actions (#231)
- server: accept GeoJSON for MSSQL geometry and geography operators (#787)
- server: update pg_dump clean output to disable function body validation in create function statements to avoid errors due to forward references
- server: fix a bug preventing some MSSQL foreign key relationships from being tracked
- console: add a comment field for actions (#231)
- console: data sidebar bug fixes and improvements (#921)
- cli: fix seeds incorrectly being applied to databases in config v3 (#6683)
- cli: add `--all-databases` flag for `migrate apply`, this allows applying migrations on all connected databases in one go
- cli-migrations: add config v3 image
- docs: add Hasura v2 upgrade guide (#1030)

## v2.0.0-alpha.6

### Support geometry and geography spatial data comparison operators in MS SQL Server

Comparison operators on spatial data types, geometry and geography, are now supported in MS SQL Server. The following operators are supported:

- STEquals
- STIntersects
- STTouches
- STOverlaps
- STCrosses
- STWithin
- STContains

**Example query:** Select values equal to a given geography instance

```
query {
  spatial_types_geog(
    where: {
      point: { _st_equals: "POINT(3 4)" }
      }
    ) {
    point
  }
}
```

**Example query:** Select values that spatially contain a given geometry instance

```
query {
  spatial_types_geom(
    where: {
      compoundcurve: { _st_contains: "POINT(0.5 0)" }
    }
  ) {
    compoundcurve
  }
}
```

### Bug fixes and improvements

- server: fix action output type schema generation (fix #6631)
- server/mssql: `mssql_add_source` can now take connection strings from environment variables
- server: support `IN`, `NIN`, `LIKE` and `NLIKE` operators in MS SQL Server
- server: remove the restriction of supporting only base type function arguments. The type of an argument with a table type is now `<tablename>_scalar` to avoid conflicts with the object type `<tablename>`.
- server: fix inherited_roles issue when some of the underlying roles don't have permissions configured (fixes #6672)
- server: fix action custom types failing to parse when mutually recursive
- server: fix MSSQL table name descriptions
- server: emit `postgres-max-connections-error` when max postgres connections are reached
- server: disable caching for actions when "forward-client-headers" option is turned on
- console: allow editing rest endpoints queries and misc ui improvements
- console: display collection names and queries from all collections in allowlist
- cli: match ordering of keys in project metadata files with server metadata

## v2.0.0-alpha.5

### Bug fixes and improvements

- server: fix issue with parsing of remote schema list of input objects (fix #6584)
- server: support tracking functions having only base type arguments (fix #6628)
- console: add browse rows for mssql tables (#805)
- console: remote schema permissions bug fixes (#439)
- cli: cli-ext is now a native part of cli binary (no longer needed as a plugin)
- cli: fix issue with adding operation to allow list in console mode (fix #6617)

## v2.0.0-alpha.4

### Bug fixes and improvements

- server/mssql: support tracking and querying from views
- server: inherited roles for PG queries and subscription
- server: replaces postgres LISTEN/NOTIFY channel with lightweight polling for metadata syncing in order to resolve proxy issues
- server: fix issue when a remote relationship's joining field had a custom GraphQL name defined (fix #6626)
- server: fix handling of nullable object relationships (fix #6633)
- console: add inherited roles support (#483)
- console: add permissions support for mssql tables (#677)
- cli: support rest endpoints
- cli: support mssql sources
- cli: use relative paths in metadata !include directives
- cli: rename `--database` flag in `migrate` and `seed` command to `--database-name`
- cli: support inherited roles


## v2.0.0-alpha.3

### Bug fixes and improvements

- server/mssql: fix malformed JSON answer on empty tables
- server/mssql: fix runtime errors when selecting geography/geometry columns
- server/mssql: supports connection pooling to sql server
- server/mssql: fix text values erroneously being parsed as varchar
- server: improve errors messages for inconsistent sources
- console: add relationship tab for mssql tables (#677)
- build: fix the packaging of static console assets (fix #6610)
- server: make REST endpoint errors compatible with inconsistent metadata


## v2.0.0-alpha.2

### MSSQL support

It's now possible to add a MSSQL server as a source. For now, only read-only queries and subscriptions are supported.

See the documentation at `graphql/core/databases/ms-sql-server` for more information.

## v2.0.0-alpha.1

Bunch of bug fixes and refactor for generalized backends: https://github.com/hasura/graphql-engine/compare/v1.4.0-alpha.2...v2.0.0-alpha.1

## v1.4.0-alpha.2

### Inconsistent Metadata

Add `allow_inconsistent_metadata` option to `replace_metadata` API.
This will replace metadata even if there are inconsistency errors,
returning a 200 response code and `is_consistent` and `inconsistent_objects`
keys in the response body.

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: fix issue of not exposing mutation functions to the admin role when function permissions are inferred (fix #6503)
- server: add "resource_version" field to metadata for concurrency control - disable lookup during migrations
- server: fix issue with queries on character column types (close #6217)
- server: optimize resolving source. Resolving a source would create connection pools every time. Optimize that to re-create connection pools only when necessary. (#609)
- server: fix issues with remote schema introspection and queries over TLS.
- server: Prohibit Invalid slashes, duplicate variables, subscriptions for REST endpoints
- server: Prohibit non-singular query definitions for REST endpoints
- server: better handling for one-to-one relationships via both `manual_configuration` and `foreign_key_constraint_on` (#2576)

## v1.4.0-alpha.1

### REST Endpoints

The RESTified GraphQL Endpoints API allows for the use of a REST interface to saved GraphQL queries and mutations.

Users specify the query or mutation they wish to make available, as well a URL template. Segments of the URL template can potentially capture data to be used as GraphQL variables.

See the documentation at `graphql/core/api-reference/restified` for more information.

### Heterogeneous execution

Previous releases have allowed queries to request data from either Postgres or remote schemas, but not both. This release removes that restriction, so multiple data sources may be mixed within a single query. For example, GraphQL Engine can execute a query like

```
query {
  articles {
    title
  }
  weather {
    temperature
  }
}
```

where the articles are fetched from the database, and the weather is fetched from a remote server.

### Support tracking VOLATILE SQL functions as mutations. (closing #1514)

Previously we could only track `STABLE` or `IMMUTABLE` functions, and only as
queries. Now the version 2 of `track_table` also supports tracking functions as
mutations:

```
  {
    "type": "track_function",
    "version": 2,
    "args": {
        "function": {
            "schema": "public",
            "name": "some_volatile_function"
        },
        "configuration": {
            "exposed_as": "mutation"
        }
      }
    }
```

### Remote schema permissions

Now, permissions can be configured for remote schemas as well, which works similar
to the permissions system of the postgres tables. Fields/arguments can be removed from the
schema and arguments can also be preset to limit the role from having unrestricted
access over it.

*NOTE*: To enable remote schema permissions, the graphql-engine needs to be started
either with the server flag ``--enable-remote-schema-permissions`` or the environment
variable ``HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS`` set to ``true``.

### Function Permissions

Before volatile functions were supported, the permissions for functions were automatically inferred
from the select permission of the target table. Now, since volatile functions are supported we can't
do this anymore, so function permissions are introduced which will explicitly grant permission to
a function for a given role. A pre-requisite to adding a function permission is that the role should
have select permissions to the target table of the function.

### `ltree` comparison operators

Comparison operators on columns with ``ltree``, ``lquery`` or ``ltxtquery`` types are now supported, for searching through data stored in a hierarchical tree-like structure.

See the documentation at `graphql/core/queries/query-filters` more details on the currently supported ``ltree`` operators.

**Example query:** Select ancestors of an `ltree` argument

```
query {
  tree (
    where: {path: {_ancestor: "Tree.Collections.Pictures.Astronomy.Astronauts"}}
  ) {
    path
  }
}
```

**Example response:**
```
{
  "data": {
    "tree": [
      {
        "path": "Tree"
      },
      {
        "path": "Tree.Collections"
      },
      {
        "path": "Tree.Collections.Pictures"
      },
      {
        "path": "Tree.Collections.Pictures.Astronomy"
      },
      {
        "path": "Tree.Collections.Pictures.Astronomy.Astronauts"
      }
    ]
  }
}
```

### Breaking changes

- This release contains the [PDV refactor (#4111)](https://github.com/hasura/graphql-engine/pull/4111), a significant rewrite of the internals of the server, which did include some breaking changes:

   - The semantics of explicit `null` values in `where` filters have changed according to the discussion in [issue 704](https://github.com/hasura/graphql-engine/issues/704#issuecomment-635571407): an explicit `null` value in a comparison input object will be treated as an error rather than resulting in the expression being evaluated to `True`. For instance: `delete_users(where: {id: {_eq: $userId}}) { name }` will yield an error if `$userId` is `null` instead of deleting all users.
   - The validation of required headers has been fixed (closing #14 and #3659):
     - if a query selects table `bar` through table `foo` via a relationship, the required permissions headers will be the union of the required headers of table `foo` and table `bar` (we used to only check the headers of the root table);
     - if an insert does not have an `on_conflict` clause, it will not require the update permissions headers.

- This release contains the remote schema permissions feature, which introduces a breaking change:

  Earlier, remote schemas were considered to be a public entity and all the roles had unrestricted
  access to the remote schema. If remote schema permissions are enabled in the graphql-engine, a given
  remote schema will only be accessible to a role ,if the role has permissions configured for the said remote schema
  and be accessible according to the permissions that were configured for the role.

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: add `request` field to webhook POST body containing the GraphQL query/mutation, its name, and any variables passed (close #2666)
- server: fix a regression where variables in fragments weren't accepted (fix #6303)
- server: output stack traces when encountering conflicting GraphQL types in the schema
- server: add `--websocket-compression` command-line flag for enabling websocket compression (fix #3292)
- server: some mutations that cannot be performed will no longer be in the schema (for instance, `delete_by_pk` mutations won't be shown to users that do not have select permissions on all primary keys) (#4111)
- server: treat the absence of `backend_only` configuration and `backend_only: false` equally (closing #5059) (#4111)
- server: accept only non-negative integers for batch size and refetch interval (close #5653) (#5759)
- server: Configurable websocket keep-alive interval. Add `--websocket-keepalive` command-line flag and `HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE` env variable (fix #3539)
- server: validate remote schema queries (fixes #4143)
- server: introduce optional custom table name in table configuration to track the table according to the custom name. The `set_table_custom_fields` API has been deprecated, A new API `set_table_customization` has been added to set the configuration. (#3811)
- server: support joining Int or String scalar types to ID scalar type in remote relationship
- server: add support for POSIX operators (close #4317) (#6172)
- server: do not block catalog migration on inconsistent metadata
- server: update `forkImmortal` function to log more information, i.e log starting of threads and log asynchronous and synchronous exception.
- server: various changes to ensure timely cleanup of background threads and other resources in the event of a SIGTERM signal.
- server: fix issue when the `relationships` field in `objects` field is passed `[]` in the `set_custom_types` API (fix #6357)
- server: fix issue with event triggers defined on a table which is partitioned (fixes #6261)
- server: action array relationships now support the same input arguments (such as where or distinct_on) as usual relationships
- server: action array relationships now support aggregate relationships
- server: fix issue with non-optional fields of the remote schema being added as optional in the graphql-engine (fix #6401)
- server: accept new config `allowed_skew` in JWT config to provide leeway for JWT expiry (fixes #2109)
- server: fix issue with query actions with relationship with permissions configured on the remote table (fix #6385)
- server: always log the `request_id` at the `detail.request_id` path for both `query-log` and `http-log` (#6244)
- server: fix issue with `--stringify-numeric-types` not stringifying aggregate fields (fix #5704)
- server: derive permissions for remote relationship field from the corresponding remote schema's permissions
- server: terminate a request if time to acquire connection from pool exceeds configurable timeout (#6326)
- server: fix issue with mapping session variables to standard JWT claims (fix #6449)
- server: support tracking of functions that return a single row (fix #4299)
- server: reduce memory usage consumption of the schema cache structures, and fix a memory leak
- server: add source name in livequery logs
- server: support ltree comparison operators (close #625)
- server: support parsing JWT from cookie header (fix #2183)
- console: allow user to cascade Postgres dependencies when dropping Postgres objects (close #5109) (#5248)
- console: mark inconsistent remote schemas in the UI (close #5093) (#5181)
- console: remove ONLY as default for ALTER TABLE in column alter operations (close #5512) #5706
- console: add onboarding helper for new users (#355)
- console: add option to flag an insertion as a migration from `Data` section (close #1766) (#4933)
- console: down migrations improvements (close #3503, #4988) (#4790)
- console: allow setting computed fields for views (close #6168) (#6174)
- console: select first operator by default on the browse rows screen (close #5729) (#6032)
- console: fix allow-list not getting added to metadata/allow_list.yaml in CLI mode (close #6374)
- console: misc bug fixes (close #4785, #6330, #6288)
- console: allow setting table custom name (#212)
- console: support tracking VOLATILE functions as mutations or queries (close #6228)
- console: show only compatible postgres functions in computed fields section (close #5155) (#5978)
- console: added export data option on browse rows page (close #1438 #5158)
- console: add session argument field for computed fields (close #5154) (#5610)
- console: add support for function permissions (#413)
- console: add tree view for Data Tab UI (#524)
- console: add support for RESTified Endpoints (#569)
- cli: add missing global flags for seed command (#5565)
- cli: allow seeds as alias for seed command (#5693)
- cli: fix action timeouts not being picked up in metadata operations (#6220)
- build: add `test_server_pg_13` to the CI to run the server tests on Postgres v13 (#6070)

## v1.3.3

### Server - Support for mapping session variables to default JWT claims

Some auth providers do not let users add custom claims in JWT. In such cases, the server can take a JWT configuration option called `claims_map` to specify a mapping of Hasura session variables to values in existing claims via JSONPath or literal values.

Example:-

Consider the following JWT claim:

```
  {
    "sub": "1234567890",
    "name": "John Doe",
    "admin": true,
    "iat": 1516239022,
    "user": {
      "id": "ujdh739kd",
      "appRoles": ["user", "editor"]
    }
  }
```

The corresponding JWT config can be:

```
  {
    "type":"RS512",
    "key": "<The public Key>",
    "claims_map": {
      "x-hasura-allowed-roles": {"path":"$.user.appRoles"},
      "x-hasura-default-role": {"path":"$.user.appRoles[0]","default":"user"},
      "x-hasura-user-id": {"path":"$.user.id"}
    }
  }
```

### Metadata Types SDK

The types and documentation comments for Metadata V2 have been converted into JSON/YAML Schema, and used to autogenerate type definitions for popular languages.

This enables users to build type-safe tooling in the language of their choice around Metadata interactions and automations.

Additionally, the JSON/YAML Schemas can be used to provide IntelliSense and autocomplete + documentation when interacting with Metadata YAML/JSON files.

For a more comprehensive overview, please see [the readme located here](./contrib/metadata-types/README.md)

**Sample Code**

```ts
import { TableEntry } from "../generated/HasuraMetadataV2";

const newTable: TableEntry = {
  table: { schema: "public", name: "user" },
  select_permissions: [
    {
      role: "user",
      permission: {
        limit: 100,
        allow_aggregations: false,
        columns: ["id", "name", "etc"],
        computed_fields: ["my_computed_field"],
        filter: {
          id: { _eq: "X-Hasura-User-ID" },
        },
      },
    },
  ],
};
```

**IntelliSense Example**

![](./contrib/metadata-types/json-schema-typecheck-demo.gif)

### Breaking changes

#### PDV

This release contains the [PDV refactor (#4111)](https://github.com/hasura/graphql-engine/pull/4111), a significant rewrite of the internals of the server, which did include some breaking changes:

- The semantics of explicit `null` values in `where` filters have changed according to the discussion in [issue 704](https://github.com/hasura/graphql-engine/issues/704#issuecomment-635571407): an explicit `null` value in a comparison input object will be treated as an error rather than resulting in the expression being evaluated to `True`. For instance: `delete_users(where: {id: {_eq: $userId}}) { name }` will yield an error if `$userId` is `null` instead of deleting all users.
- The validation of required headers has been fixed (closing #14 and #3659):
  - if a query selects table `bar` through table `foo` via a relationship, the required permissions headers will be the union of the required headers of table `foo` and table `bar` (we used to only check the headers of the root table);
  - if an insert does not have an `on_conflict` clause, it will not require the update permissions headers.

#### Remote Relationship

In this release, a breaking change has been introduced:

In a remote relationship query, the remote schema will be queried when all of the joining arguments
are **not** `null` values. When there are `null` value(s), the remote schema won't be queried and the
response of the remote relationship field will be `null`. Earlier, the remote schema
was queried with the `null` value arguments and the response depended upon how the remote schema handled the `null`
arguments.

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: allow remote relationships joining `type` column with `[type]` input argument as spec allows this coercion (fixes #5133)
- server: add action-like URL templating for event triggers and remote schemas (fixes #2483)
- server: change `created_at` column type from `timestamp` to `timestamptz` for scheduled triggers tables (fix #5722)
- server: allow configuring timeouts for actions (fixes #4966)
- server: fix bug which arised when renaming a table which had a manual relationship defined (close #4158)
- server: limit the length of event trigger names (close #5786)
  **NOTE:** If you have event triggers with names greater than 42 chars, then you should update their names to avoid running into Postgres identifier limit bug (#5786)
- server: enable HASURA_GRAPHQL_PG_CONN_LIFETIME by default to reclaim memory
- server: fix issue with tracking custom functions that return `SETOF` materialized view (close #5294) (#5945)
- server: allow remote relationships with union, interface and enum type fields as well (fixes #5875) (#6080)
- server: Fix fine-grained incremental cache invalidation (fix #6027)
  This issue could cause enum table values to sometimes not be properly reloaded without restarting `graphql-engine`. Now a `reload_metadata` API call (or clicking “Reload enum values” in the console) should consistently force a reload of all enum table values.
- server: fix event trigger cleanup on deletion via replace_metadata (fix #5461) (#6137)
  **WARNING**: This can cause significant load on PG on startup if you have lots of event triggers. Delay in starting up is expected.
- console: add notifications (#5070)
- cli: fix bug in metadata apply which made the server aquire some redundant and unnecessary locks (close #6115)
- cli: fix cli-migrations-v2 image failing to run as a non root user (close #4651, close #5333)
- cli: fix issue with cli binary on latest Mac (Big Sur) (fix #5462)
- docs: add docs page on networking with docker (close #4346) (#4811)
- docs: add tabs for console / cli / api workflows (close #3593) (#4948)
- docs: add postgres concepts page to docs (close #4440) (#4471)
- docs: add guides on connecting hasura cloud to pg databases of different cloud vendors (#5948)

## `v1.3.2`

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: fixes column masking in select permission for computed fields regression (fix #5696)

## `v1.3.1`, `v1.3.1-beta.1`

### Breaking change

Headers from environment variables starting with `HASURA_GRAPHQL_` are not allowed
in event triggers, actions & remote schemas.

If you do have such headers configured, then you must update the header configuration before upgrading.

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: fix failing introspection query when an enum column is part of a primary key (fixes #5200)
- server: disallow headers from env variables starting with `HASURA_GRAPHQL_` in actions, event triggers & remote schemas (#5519)
  **WARNING**: This might break certain deployments. See `Breaking change` section above.
- server: bugfix to allow HASURA_GRAPHQL_QUERY_PLAN_CACHE_SIZE of 0 (#5363)
- server: support only a bounded plan cache, with a default size of 4000 (closes #5363)
- server: add logs for action handlers
- server: add request/response sizes in event triggers (and scheduled trigger) logs (#5463)
- server: change startup log kind `db_migrate` to `catalog_migrate` (#5531)
- console: handle nested fragments in allowed queries (close #5137) (#5252)
- console: update sidebar icons for different action and trigger types (#5445)
- console: make add column UX consistent with others (#5486)
- console: add "identity" to frequently used columns (close #4279) (#5360)
- cli: improve error messages thrown when metadata apply fails (#5513)
- cli: fix issue with creating seed migrations while using tables with capital letters (closes #5532) (#5549)
- build: introduce additional log kinds for cli-migrations image (#5529)

## `v1.3.0`

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: adjustments to idle GC to try to free memory more eagerly (related to #3388)
- server: process events generated by the event triggers asynchronously (close #5189) (#5352)
- console: display line number that error originated from in GraphQL editor (close #4849) (#4942)
- docs: add page on created_at / updated_at timestamps (close #2880) (#5223)

## `v1.3.0-beta.4`

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: change relay endpoint to `/v1beta1/relay` (#5257)
- server: relay connection fields are exposed regardless of allow aggregation permission (fix #5218) (#5257)
- server: add new `--conn-lifetime` and `HASURA_GRAPHQL_PG_CONN_LIFETIME` options for expiring connections after some amount of active time (#5087)
- server: shrink libpq connection request/response buffers back to 1MB if they grow beyond 2MB, fixing leak-like behavior on active servers (#5087)
- server: have haskell runtime release blocks of memory back to the OS eagerly (related to #3388)
- server: unlock locked scheduled events on graceful shutdown (#4928)
- server: disable prepared statements for mutations as we end up with single-use objects which result in excessive memory consumption for mutation heavy workloads (#5255)
- server: include scheduled event metadata (`created_at`,`scheduled_time`,`id`, etc) along with the configured payload in the request body to the webhook.
  **WARNING:** This is breaking for beta versions as the payload is now inside a key called `payload`.
- console: allow configuring statement timeout on console RawSQL page (close #4998) (#5045)
- console: support tracking partitioned tables (close #5071) (#5258)
- console: add button to cancel one-off scheduled events and cron-trigger events (close #5161) (#5236)
- console: handle generated and identity columns in console data section (close #4552, #4863) (#4761)
- cli: fix plugins install failing due to permission issues on windows (close #5111)
- docs: add note for managed databases in postgres requirements (close #1677, #3783) (#5228)
- docs: add 1-click deployment to Nhost page to the deployment guides (#5180)
- docs: add hasura cloud to getting started section (close #5206) (#5208)

## `v1.3.0-beta.3`

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: fix introspection when multiple actions defined with Postgres scalar types (fix #5166) (#5173)
- console: allow manual edit of column types and handle array data types (close #2544, #3335, #2583) (#4546)
- console: add the ability to delete a role in permissions summary page (close #3353) (#4987)
- console: fix styling of table row contents on tables on relationship page (#4974)
- cli: handle missing files during metadata apply (close #5163) (#5170)
- docs: add pages on remote joins (close #4911) (#5132)
- docs: add page on scheduled triggers (close #4913) (#5141)
- docs: add page on Relay schema (close #4912) (#5150)

## `v1.3.0-beta.2`

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: add `--pg-connection-options` command-line flag for passing parameters to PostgreSQL (close #5092) (#5187)
- server: improve memory usage of idle websockets connections (#5190)
- server: few relay fixes (fix #5020, #5037, #5046) (#5013)
- server: raise error on startup when `--unauthorized-role` is ignored (#4736)
- server: fix bug which arises when renaming/dropping a column on a remote relationship (#5005, #5119)
- console: provide option to cascade metadata on dependency conflicts on console (fix #1593)
- console: fix enum tables reload data button UI (#4647)
- console: fix "Cannot read property 'foldable'" runtime error in Browse Rows page (fix #4907) (#5016)
- console: respect read-only mode in actions pages (fix #4656) (#4764)
- console: allow configuring session_argument for custom functions (close #4499) (#4922)
- console: fix listen update column config selection for event trigger (close #5042) (#5043)
- cli: add new flags up-sql and down-sql to generate sql based migrations from the CLI (#5026)
- docs: add instructions on fixing loss of data when using floats (close #5092)
- docs: add page on setting up v2 migrations (close #4746) (#4898)

## `v1.3.0-beta.1`

### Relay

The Hasura GraphQL Engine serves [Relay](https://relay.dev/) schema for Postgres tables which has a primary key defined.

The Relay schema can be accessed through `/v1beta1/relay` endpoint.

[Add docs links][add console screenshot for relay toggle]

### Remote Joins

Remote Joins extend the concept of joining data across tables, to being able to join data across tables and remote schemas.

It works similar to table relationships. Head to the `Relationship` tab in your table page and define a remote relationship:

1. give a name for the relationship
2. select the remote schema
3. give the join configuration from table columns to remote schema fields.

[Add docs links][add console screenshot]

### Scheduled Triggers

A scheduled trigger can be used to execute custom business logic based on time. There are two types of timing events: cron based or timestamp based.

A cron trigger will be useful when something needs to be done periodically. For example, you can create a cron trigger to generate an end-of-day sales report every weekday at 9pm.

You can also schedule one-off events based on a timestamp. For example, a new scheduled event can be created for 2 weeks from when a user signs up to send them an email about their experience.

[Add docs links][add console screenshot]

(close #1914)

### Allow access to session variables by computed fields (fix #3846)

Sometimes it is useful for computed fields to have access to the Hasura session variables directly. For example, suppose you want to fetch some articles but also get related user info, say `likedByMe`. Now, you can define a function like:

```
CREATE OR REPLACE FUNCTION article_liked(article_row article, hasura_session json)
RETURNS boolean AS $$
  SELECT EXISTS (
    SELECT 1
    FROM liked_article A
    WHERE A.user_id = hasura_session ->> 'x-hasura-user-id' AND A.article_id = article_row.id
  );
$$ LANGUAGE sql STABLE;
```

and make a query like:

```
query {
  articles {
    title
    content
    likedByMe
  }
}
```

Support for this is now added through the `add_computed_field` API.

Read more about the session argument for computed fields in the [docs](https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html).

### Manage seed migrations as SQL files

A new `seeds` command is introduced in CLI, this will allow managing seed migrations as SQL files

#### Creating seed

```
# create a new seed file and use editor to add SQL content
hasura seed create new_table_seed

# create a new seed by exporting data from tables already present in the database
hasura seed create table1_seed --from-table table1

# create from data in multiple tables:
hasura seed create tables_seed --from-table table1 --from-table table2
```

#### Applying seed

```
# apply all seeds on the database:
hasura seed apply

# apply only a particular seed
hasura seed apply --file 1234_add_some_seed_data.sql
```

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: fix explain queries with role permissions (fix #4816)
- server: compile with GHC 8.10.1, closing a space leak with subscriptions. (close #4517) (#3388)
- server: fixes an issue where introspection queries with variables would fail because of caching (fix #4547)
- server: avoid loss of precision when passing values in scientific notation (fix #4733)
- server: fix mishandling of GeoJSON inputs in subscriptions (fix #3239)
- server: fix importing of allow list query from metadata (fix #4687)
- server: flush log buffer during shutdown (#4800)
- server: fix edge case with printing logs on startup failure (fix #4772)
- console: allow entering big int values in the console (close #3667) (#4775)
- console: add support for subscriptions analyze in API explorer (close #2541) (#2541)
- console: avoid count queries for large tables (#4692)
- console: add read replica support section to pro popup (#4118)
- console: fix regression in editing permissions manually (fix #4683) (#4826)
- console: allow modifying default value for PK (fix #4075) (#4679)
- console: fix checkbox for forwarding client headers in actions (#4595)
- console: re-enable foreign tables to be listed as views (fix #4714) (#4742)
- console: display rows limit in permissions editor if set to zero (fix #4559)
- console: fix inconsistency between selected rows state and displayed rows (fix #4654) (#4673)
- console: fix displaying boolean values in `Edit Row` tab (#4682)
- console: fix underscores not being displayed on raw sql page (close #4754) (#4799)
- console: fix visiting view modify page overwriting raw sql content (fix #4798) (#4810)
- console: add help button and move about page to settings (#4848)
- console: add new sidebar icon that separates enums from tables (fix #4984) (#4992)
- cli: list all available commands in root command help (fix #4623) (#4628)
- cli: fix bug with squashing event triggers (close #4883)
- cli: add support for skipping execution while generating migrations through the migrate REST API
- cli: add dry run flag in hasura migrate apply command (fix #3128) (#3499)
- cli: load assets from server when HASURA_GRAPHQL_CONSOLE_ASSETS_DIR is set (close #3382)
- docs: add section on actions vs. remote schemas to actions documentation (#4284)
- docs: fix wrong info about excluding scheme in CORS config (#4685)
- docs: add single object mutations docs (close #4622) (#4625)
- docs: add docs page on query performance (close #2316) (#3693)
- docs: add a sample Caddyfile for Caddy 2 in enable-https section (#4710)
- docs: add disabling dev mode to production checklist (#4715)
- docs: add integration guide for AWS Cognito (#4822, #4843)
- docs: update troubleshooting section with reference on debugging errors (close #4052) (#4825)
- docs: add page for procuring custom docker images and binaries (#4828)
- docs: add content on how to secure action handlers and other actions docs improvements (#4743)
- docs: make header common with other hasura.io/ pages (#4957)
- install manifests: update all install manifests to enable dev mode by default (close #4599) (#4716)

## `v1.2.0`

Include the changelog from **v1.2.0-beta.1**, **v1.2.0-beta.2**, **v1.2.0-beta.3**, **v1.2.0-beta.4**, **v1.2.0-beta.5**

Additional changelog:

### CLI: Support servers with self-signed certificates (close #4564) (#4582)

A new flag `--certificate-authority` is added so that the CA certificate can be
provided to trust the Hasura Endpoint with a self-signed SSL certificate.

Another flag `--insecure-skip-tls-verification` is added to skip verifying the certificate
in case you don't have access to the CA certificate. As the name suggests,
using this flag is insecure since verification is not carried out.

### Bug fixes and improvements

- console: update graphiql explorer to support operation transform (#4567)
- console: make GraphiQL Explorer taking the whole viewport (#4553)
- console: fix table columns type comparision during column edit (close #4125) (#4393)
- cli: allow initialising project in current directory (fix #4560) #4566
- cli: remove irrelevant flags from init command (close #4508) (#4549)
- docs: update migrations docs with config v2 (#4586)
- docs: update actions docs (#4586)

## `v1.2.0-beta.5`

### server: backend only insert permissions

Introduces optional `backend_only` (default: `false`) configuration in insert permissions
(see [api reference](https://deploy-preview-4224--hasura-docs.netlify.com/graphql/manual/api-reference/schema-metadata-api/permission.html#insertpermission)).
If this is set to `true`, the insert mutation is accessible to the role only if the request
is accompanied by `x-hasura-use-backend-only-permissions` session variable whose value is set to `true` along with the `x-hasura-admin-secret` header.
Otherwise, the behavior of the permission remains unchanged.

This feature is highly useful in disabling `insert_table` mutation for a role from frontend clients while still being able to access it from a Action webhook handler (with the same role).

(rfc #4120) (#4224)

### server: debugging mode for non-admin roles

For any errors the server sends extra information in `extensions` field under `internal` key. Till now this was only
available for `admin` role requests. To enable this for other roles, start the server with `--dev-mode` flag or set `HASURA_GRAPHQL_DEV_MODE` env variable to `true`:

```bash
$ graphql-engine --database-url <database-url> serve --dev-mode
```

In case you want to disable `internal` field for `admin` role requests, set `--admin-internal-errors` option to `false` or or set `HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS` env variable to `false`

```bash
$ graphql-engine --database-url <database-url> serve --admin-internal-errors false
```

This feature come in handy during development when you may want to see detailed errors irrespective of roles.

**Improved internal errors for Actions**:

(This is a **breaking change** with previous 1.2.0-beta releases)

The `internal` field for action errors is improved with more debug information. It now includes `request`,
`response` and `error` fields instead of just `webhook_response` field.

Before:

```json
{
  "errors": [
    {
      "extensions": {
        "internal": {
          "webhook_response": {
            "age": 25,
            "name": "Alice",
            "id": "some-id"
          }
        },
        "path": "$",
        "code": "unexpected"
      },
      "message": "unexpected fields in webhook response: age"
    }
  ]
}
```

After:

```json
{
  "errors": [
    {
      "extensions": {
        "internal": {
          "error": "unexpected response",
          "response": {
            "status": 200,
            "body": {
              "age": 25,
              "name": "Alice",
              "id": "some-id"
            },
            "headers": [
              {
                "value": "application/json",
                "name": "Content-Type"
              },
              {
                "value": "abcd",
                "name": "Set-Cookie"
              }
            ]
          },
          "request": {
            "body": {
              "session_variables": {
                "x-hasura-role": "admin"
              },
              "input": {
                "arg": {
                  "age": 25,
                  "name": "Alice",
                  "id": "some-id"
                }
              },
              "action": {
                "name": "mirror"
              }
            },
            "url": "http://127.0.0.1:5593/mirror-action",
            "headers": []
          }
        },
        "path": "$",
        "code": "unexpected"
      },
      "message": "unexpected fields in webhook response: age"
    }
  ]
}
```

### cli: add support for .env file

ENV vars can now be read from .env file present at the project root directory. A global flag, `--envfile`, is added so you can explicitly provide the .env filename, which defaults to `.env` filename if no flag is provided.

**Example**:

```
hasura console --envfile production.env
```

The above command will read ENV vars from `production.env` file present at the project root directory.

(close #4129) (#4454)

### console: allow setting post-update check in update permissions

Along with the check for filtering rows that can be updated, you can now set a post-update permission check that needs to be satisfied by the updated rows after the update is made.

<add-screenshot>

(close #4142) (#4313)

### console: support for Postgres [materialized views](https://www.postgresql.org/docs/current/rules-materializedviews.html)

Postgres materialized views are views that are persisted in a table-like form. They are now supported in the Hasura Console, in the same way as views. They will appear on the 'Schema' page, under the 'Data' tab, in the 'Untracked tables or views' section.

(close #91) (#4270)

### docs: map Postgres operators to corresponding Hasura operators

Map Postgres operators to corresponding Hasura operators at various places in docs and link to PG documentation for reference.
For example, see [here](https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#operator).

(#4502) (close #4056)

### Bug fixes and improvements

- server: add support for `_inc` on `real`, `double`, `numeric` and `money` (fix #3573)
- server: support special characters in JSON path query argument with bracket `[]` notation, e.g `obj['Hello World!']` (#3890) (#4482)
- server: add graphql-engine support for timestamps without timezones (fix #1217)
- server: support inserting unquoted bigint, and throw an error if value overflows the bounds of the integer type (fix #576) (fix #4368)
- console: change react ace editor theme to eclipse (close #4437)
- console: fix columns reordering for relationship tables in data browser (#4483)
- console: format row count in data browser for readablity (#4433)
- console: move pre-release notification tooltip msg to top (#4433)
- console: remove extra localPresets key present in migration files on permissions change (close #3976) (#4433)
- console: make nullable and unique labels for columns clickable in insert and modify (#4433)
- console: fix row delete for relationships in data browser (#4433)
- console: prevent trailing spaces while creating new role (close #3871) (#4497)
- docs: add API docs for using environment variables as webhook urls in event triggers
- server: fix recreating action's permissions (close #4377)
- server: make the graceful shutdown logic customizable (graceful shutdown on the SIGTERM signal continues to be the default)
- docs: add reference docs for CLI (clsoe #4327) (#4408)

## `v1.2.0-beta.4`

### add query support in actions

(close #4032) (#4309)

### console: persist page state in data browser across navigation

The order, collapsed state of columns and rows limit is now persisted across page navigation

(close #3390) (#3753)

### Bug fixes and improvements

- cli: query support for actions (#4318)
- cli: add retry_conf in event trigger for squashing migrations (close #4296) (#4324)
- cli: allow customization of server api paths (close #4016)
- cli: clean up migration files created during a failed migrate api (close #4312) (#4319)
- cli: add support for multiple versions of plugin (close #4105)
- cli: template assets path in console HTML for unversioned builds
- cli: set_table_is_enum metadata type for squashing migrations (close #4394) (#4395)
- console: query support for actions (#4318)
- console: recover from SDL parse in actions type definition editor (fix #4385) (#4389)
- console: allow customising graphql field names for columns of views (close #3689) (#4255)
- console: fix clone permission migrations (close #3985) (#4277)
- console: decouple data rows and count fetch in data browser to account for really large tables (close #3793) (#4269)
- console: update cookie policy for API calls to "same-origin"
- console: redirect to /:table/browse from /:table (close #4330) (#4374)
- console: surround string type column default value with quotes (close #4371) (#4423)
- console: add undefined check to fix error (close #4444) (#4445)
- docs: add One-Click Render deployment guide (close #3683) (#4209)
- server: reserved keywords in column references break parser (fix #3597) #3927
- server: fix postgres specific error message that exposed database type on invalid query parameters (#4294)
- server: manage inflight events when HGE instance is gracefully shutdown (close #3548)
- server: fix an edge case where some events wouldn't be processed because of internal erorrs (#4213)
- server: fix downgrade not working to version v1.1.1 (#4354)
- server: `type` field is not required if `jwk_url` is provided in JWT config
- server: add a new field `claims_namespace_path` which accepts a JSON Path for looking up hasura claim in the JWT token (#4349)
- server: support reusing Postgres scalars in custom types (close #4125)

## `v1.2.0-beta.3`

### console: manage Postgres check constraints

Postgres Check constraints allows you to specify that the value in a certain column must satisfy a Boolean (truth-value) expression. They can be used to put in simple input validations for mutations and with this release, these constraints can now be added while creating a table or later from Modify tab on the console.

**Example**:
When a product is created, ensure that the price is greater than zero. The SQL would look like this:

```sql
CREATE TABLE products (
    product_id UUID DEFAULT gen_random_uuid(),
    name TEXT,
    price NUMERIC CONSTRAINT positive_price CHECK (price > 0)
);
```

To create this table with Hasura Console, on the 'Add a new table' screen, after adding all the columns, scroll down to 'Check constraints' section and 'Add a new check constraint' with the following properties:

- Constraint name: `positive_price`
- Check expression: `price > 0`

Read more about check constraints on [Postgres Docs](https://www.postgresql.org/docs/12/ddl-constraints.html#DDL-CONSTRAINTS-CHECK-CONSTRAINTS).

(close #1700) (#3881)

### CLI: V2 migrations architecture

A new CLI migrations image is introduced to account for the new migrations workflow. If you're have a project with `version: 2` in `config.yaml`, you should use the new image: `hasura/graphql-engine:v1.2.0-cli-migrations-v2`. Mount the migrations at `/hasura-migrations` and metadata at `/hasura-metadata`.

See [upgrade docs](https://hasura.io/docs/latest/graphql/core/migrations/upgrade-v2.html).

(close #3969) (#4145)

### Bug fixes and improvements

- server: improve performance of replace_metadata tracking many tables (fix #3802)
- server: option to reload remote schemas in 'reload_metadata' API (fix #3792, #4117)
- server: fix various space leaks to avoid excessive memory consumption
- server: fix postgres query error when computed fields included in mutation response (fix #4035)
- server: fix `__typename` not being included for custom object types (fix #4063)
- server: preserve cookie headers from sync action webhook (close #4021)
- server: validate action webhook response to conform to action output type (fix #3977)
- server: add 'ID' to default scalars in custom types (fix #4061)
- server: fix erroneous error log "Received STOP for an operation ..."
- console: enum field values can be selected through a dropdown in insert/edit rows page (close #3748) (#3810)
- console: exported metadata filenames are now unique(`hasura_metadata_<timestamp>.json`) (close #1772) (#4106)
- console: allow bulk deleting rows in 'Browse Rows' section (close #1739) (#3735)
- console: fix computed field permission selection (#4246)
- console: allow customising root fields of single row mutations (close #4203) (#4254)
- console: fix json string rendering in data browser (close #4201) (#4221)
- console: handle long column names in event trigger update columns (close #4123) (#4210)
- console: disable selecting roles without permissions for bulk actions (close #4178) (#4195)
- console: fix passing default value to JsonInput (#4175)
- console: fix parsing of wrapped types in SDL (close #4099) (#4167)
- console: misc actions fixes (#4059)
- console: action relationship page improvements (fix #4062, #4130) (#4133)
- console: add code exporter to graphiql (close #4531) #4652
- cli: fix init command to generate correct config (fix #4036) (#4038)
- cli: fix parse error returned on console api (close #4126) (#4152)
- cli: fix typo in cli example for squash (fix #4047) (#4049)
- docs: add statement to grant hasura permissions for PG functions (#4238)
- docs: add docs for redeliver_event api (fix #4176) (#4177)
- docs: update permission.rst for check constraint api (#4124)
- docs: add note on pg versions for actions (#4034)
- docs: add latest prerelease build info (close #4041) (#4048)
- docs: add AuthGuardian JWT guide (#3958)

## `v1.2.0-beta.2`

- server: Don't update catalog version if using --dryRun (#3970)
- cli: add version flag in update-cli command (#3996)
- cli(migrations-img): add env to skip update prompts (fix #3964) (#3968)
- cli, server: use prerelease tag as channel for console assets cdn (#3975)
- cli: fix flags in actions, migrate and metadata cmd (fix #3982) (#3991)
- cli: preserve action definition in metadata apply (fix… (#3993)
- cli: bug fixes related to actions (#3951)

## `v1.2.0-beta.1`

### Hasura Actions

Actions are a way to extend Hasura’s auto-generated mutations with entirely custom ones which can handle various use cases such as data validation, data enrichment from external sources and any other complex business logic.

A new mutation can be created either by defining its GraphQL SDL or by deriving it from an existing Hasura-generated mutation. The resolver is exposed to Hasura as a webhook which can be called synchronously or asynchronously. This release also includes an ever evolving codegen workflow to make managing the custom resolvers easier.

Read more about actions in the [docs](https://docs.hasura.io/1.0/graphql/manual/actions/index.html).

(#3042) (#3252) (#3859)

### Downgrade command

A new command is added to the server executable for downgrading to earlier releases. Previously, if you ran a newer Hasura version and wanted to go back to an old version on the same database, you had to stop Hasura, run some SQL statements and start Hasura again. With the new `downgrade` command, these SQL statements can be run automatically.

**Example**: Downgrade from `v1.2.0` to `v1.0.0`:

```bash
# stop hasura v1.2.0

# run the following command:
docker run hasura/graphql-engine:v1.2.0 graphql-engine --database-url <db-url> downgrade --to-v1.0.0

# start hasura v1.0.0
```

Read more about this command in the [docs](https://hasura.io/docs/latest/graphql/core/deployment/downgrading.html#downgrading-hasura-graphql-engine).

(close #1156) (#3760)

### Expiration of connections authenticated by WebHooks

When using webhooks to authenticate incoming requests to the GraphQL engine server, it is now possible to specify an expiration time; the connection to the server will be automatically closed if it's still running when the expiration delay is expired.

Read more about it in the [docs](https://hasura.io/docs/latest/graphql/core/auth/authentication/webhook.html).

### Bug fixes and improvements

- server: check expression in update permissions (close #384) (rfc #3750) (#3804)
- console: show pre-release update notifications with opt out option (#3888)
- console: handle invalid keys in permission builder (close #3848) (#3863)
- docs: add page on data validation to docs (close #4085) (#4260)
