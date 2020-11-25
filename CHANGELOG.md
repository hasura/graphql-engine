# Hasura GraphQL Engine Changelog

## Next release

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

### Breaking changes

This release contains the [PDV refactor (#4111)](https://github.com/hasura/graphql-engine/pull/4111), a significant rewrite of the internals of the server, which did include some breaking changes:

- The semantics of explicit `null` values in `where` filters have changed according to the discussion in [issue 704](https://github.com/hasura/graphql-engine/issues/704#issuecomment-635571407): an explicit `null` value in a comparison input object will be treated as an error rather than resulting in the expression being evaluated to `True`. For instance: `delete_users(where: {id: {_eq: $userId}}) { name }` will yield an error if `$userId` is `null` instead of deleting all users.
- The validation of required headers has been fixed (closing #14 and #3659):
  - if a query selects table `bar` through table `foo` via a relationship, the required permissions headers will be the union of the required headers of table `foo` and table `bar` (we used to only check the headers of the root table);
  - if an insert does not have an `on_conflict` clause, it will not require the update permissions headers.

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)

- server: add `--websocket-compression` command-line flag for enabling websocket compression (fix #3292)
- server: some mutations that cannot be performed will no longer be in the schema (for instance, `delete_by_pk` mutations won't be shown to users that do not have select permissions on all primary keys) (#4111)
- server: treat the absence of `backend_only` configuration and `backend_only: false` equally (closing #5059) (#4111)
- server: accept only non-negative integers for batch size and refetch interval (close #5653) (#5759)
- server: Configurable websocket keep-alive interval. Add `--websocket-keepalive` command-line flag and `HASURA_GRAPHQL_WEBSOCKET_KEEPALIVE` env variable (fix #3539)
- server: validate remote schema queries (fixes #4143)
- server: introduce optional custom table name in table configuration to track the table according to the custom name. The `set_table_custom_fields` API has been deprecated, A new API `set_table_customization` has been added to set the configuration. (#3811)
- server: support joining Int or String scalar types to ID scalar type in remote relationship
- console: allow user to cascade Postgres dependencies when dropping Postgres objects (close #5109) (#5248)
- console: mark inconsistent remote schemas in the UI (close #5093) (#5181)
- console: remove ONLY as default for ALTER TABLE in column alter operations (close #5512) #5706
- console: add option to flag an insertion as a migration from `Data` section (close #1766) (#4933)
- console: down migrations improvements (close #3503, #4988) (#4790)
- console: allow setting computed fields for views (close #6168) (#6174)
- console: select first operator by default on the browse rows screen (close #5729) (#6032)
- cli: add missing global flags for seed command (#5565)
- cli: allow seeds as alias for seed command (#5693)

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
import { TableEntry } from "../generated/HasuraMetadataV2"

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
}
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

The Hasura GraphQL Engine serves [Relay](https://relay.dev/en/) schema for Postgres tables which has a primary key defined.

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

Read more about the session argument for computed fields in the [docs](https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html).

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
For example, see [here](https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#operator).

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

See [upgrade docs](https://hasura.io/docs/1.0/graphql/manual/migrations/upgrade-v2.html).

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

Read more about this command in the [docs](https://hasura.io/docs/1.0/graphql/manual/deployment/downgrading.html#downgrading-hasura-graphql-engine).

(close #1156) (#3760)

### Expiration of connections authenticated by WebHooks

When using webhooks to authenticate incoming requests to the GraphQL engine server, it is now possible to specify an expiration time; the connection to the server will be automatically closed if it's still running when the expiration delay is expired.

Read more about it in the [docs](https://hasura.io/docs/1.0/graphql/manual/auth/authentication/webhook.html).

### Bug fixes and improvements

- server: check expression in update permissions (close #384) (rfc #3750) (#3804)
- console: show pre-release update notifications with opt out option (#3888)
- console: handle invalid keys in permission builder (close #3848) (#3863)
- docs: add page on data validation to docs (close #4085) (#4260)
