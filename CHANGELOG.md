# Hasura GraphQL Engine Changelog

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

### Bug fixes and improvements
- server: check expression in update permissions (close #384) (rfc #3750) (#3804)
- console: show pre-release update notifications with opt out option (#3888)
- console: handle invalid keys in permission builder (close #3848) (#3863)
- Check expression in update permissions (close #384) (rfc #3750) (#3804) 

- console: add multi select in browse rows to allow bulk delete (close #1739) (#3735)

  Adds a checkbox to each row on Browse Rows view that allows selecting one or more rows from the table and bulk delete them.

- console: allow setting check constraints during table create (#3881)

  Adds a component that allows adding check constraints while creating a new table in the same way as it can be done on the `Modify` view.

- console: add dropdown for enum fields in insert/edit rows page (close #3748) (#3810)

  If a table has a field referencing an enum table via a foreign key, then there will be a select dropdown with all possible enum values for that field on `Insert Row` and `Edit Row` views.

- console: generate unique exported metadata filenames (close #1772) (#4106)

  Exporting metadata from the console will now generate metadata files of the form `hasura_metadata_<timestamp>.json`.

- cli(migrations-docker): add support for v2 config (close #3969)

  A new CLI migrations image is introduced to account for the new CLI workflow. If you're have a project with `version: 2` in `config.yaml`, you should use the new image: `hasura/graphql-engine:v1.2.0-cli-migrations-v2`. Mount the migrations at `/hasura-migrations` and metadata at `/hasura-metadata`.
