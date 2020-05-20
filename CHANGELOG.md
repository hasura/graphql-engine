# Hasura GraphQL Engine Changelog

## Next release

### Scheduled Triggers

A scheduled trigger can be used to execute custom business logic based on time. There are two types of timing events: cron based or timestamp based.

A cron trigger will be useful when something needs to be done periodically. For example, you can create a cron trigger to  generate an end-of-day sales report every weekday at 9pm.

You can also schedule one-off events based on a timestamp. For example, a new scheduled event can be created for 2 weeks from when a user signs up to send them an email about their experience. 

<Add docs links>

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

### Bug fixes and improvements

(Add entries here in the order of: server, console, cli, docs, others)
- server: compile with GHC 8.10.1, closing a space leak with subscriptions. (close #4517) (#3388)

- server: fixes an issue where introspection queries with variables would fail because of caching (fix #4547)
- server: avoid loss of precision when passing values in scientific notation (fix #4733)
- server: fix mishandling of GeoJSON inputs in subscriptions (fix #3239)
- server: flush log buffer during shutdown (#4800)
- console: fix editing permissions manually seems to have stopped working properly (fix #4683)
- console: avoid count queries for large tables (#4692)
- console: add read replica support section to pro popup (#4118)
- console: allow modifying default value for PK (fix #4075) (#4679)
- console: fix checkbox for forwarding client headers in actions (#4595)
- console: re-enable foreign tables to be listed as views (fix #4714) (#4742)
- console: display rows limit in permissions editor if set to zero (fix #4559)
- console: fix inconsistency between selected rows state and displayed rows (fix #4654) (#4673)
- console: fix displaying boolean values in `Edit Row` tab (#4682)
- console: fix underscores not being displayed on raw sql page (close #4754) (#4799)
- cli: list all available commands in root command help (fix #4623) (#4628)
- docs: add section on actions vs. remote schemas to actions documentation (#4284)
- docs: fix wrong info about excluding scheme in CORS config (#4685)
- docs: add single object mutations docs (close #4622) (#4625)
- docs: add docs page on query performance (close #2316) (#3693)
- docs: add a sample Caddyfile for Caddy 2 in enable-https section (#4710)
- docs: add disabling dev mode to production checklist (#4715)

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
