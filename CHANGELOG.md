# Hasura GraphQL Engine Changelog

## `v1.2.0-beta.3`

### Manage Postgres Check Constraints from Console

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

### Bug fixes and improvements
- server: option to reload remote schemas in 'reload_metadata' API (fix #3792, #4117)
- server: fix various space leaks to avoid excessive memory consumption
- server: fix postgres query error when computed fields included in mutation response (fix #4035)
- server: fix `__typename` not being included for custom object types (fix #4063)
- server: preserve cookie headers from sync action webhook (close #4021)
- server: validate action webhook response to conform to action output type (fix #3977)
- server: add 'ID' to default scalars in custom types (fix #4061)

## `v1.2.0-beta.2`

- server: Don't update catalog version if using --dryRun (#3970)
- cli: bug fixes related to actions (#3951)
- cli(actions): preserve action definition in metadata apply (fix… (#3993)
- cli: fix flags in actions, migrate and metadata cmd (fix #3982) (#3991)
- cli: add version flag in update-cli command (#3996)

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

### V2 migrations architecture

- cli(migrations-docker): add support for v2 config (close #3969)

  A new CLI migrations image is introduced to account for the new CLI workflow. If you're have a project with `version: 2` in `config.yaml`, you should use the new image: `hasura/graphql-engine:v1.2.0-cli-migrations-v2`. Mount the migrations at `/hasura-migrations` and metadata at `/hasura-metadata`.

### Bug fixes and improvements
- server: check expression in update permissions (close #384) (rfc #3750) (#3804)
- console: add multi select in browse rows to allow bulk delete (close #1739) (#3735)
- console: add dropdown for enum fields in insert/edit rows page (close #3748) (#3810)
- console: exported metadata file name will now be of format `hasura_metadata_<timestamp.json` (close #1772) (#4106)
- console: handle invalid keys in permission builder (close #3848) (#3863)
- cli(actions): preserve action definition in metadata apply (fix #3988) (#3993)
- docs: add AuthGuardian JWT guide (#3958)
- docs: document `redeliver_event` API
