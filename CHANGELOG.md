# Hasura GraphQL Engine Changelog

## `v1.2.0` (in beta)

### Hasura Actions

Actions are a way to extend Hasura’s auto-generated mutations with entirely custom ones which can handle various use cases such as data validation, data enrichment from external sources and any other complex business logic.

A new mutation can be created either by defining its GraphQL SDL or by deriving it from an existing Hasura-generated mutation. The resolver is exposed to Hasura as a webhook which can be called synchronously or asynchronously. This release also includes an ever evolving codegen workflow to make managing the custom resolvers easier.

Read more about actions in the [docs](https://docs.hasura.io/1.0/graphql/manual/actions/index.html).

(#3042) (#3252) (#3859)

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
  
### Other changes

- cli: fix typo in cli example for squash (fix #4047) (#4049)
- console: fix run_sql migration modal messaging (close #4020) (#4060)
- docs: add note on pg versions for actions (#4034)
- console: update actions intro image (#4042)
- console: hide starter kit button if a framework has no starter kit (#4023)
- docs: avoid redirect, update title tag suffix (#4030)
- More robust forking, exception safety. Closes #3768 (#3860)
- tag release v1.2.0-beta.2 (#4028)
- docs: correct version info for config v2 section (close #4019) (#4026)
- console: add TypeScript setup (#3902)
- cli: add version flag in update-cli command (#3996)
- cli(migrations-img): add env to skip update prompts (fix #3964) (#3968)
- Don't update catalog version if using --dryRun (#3970)
- cli, server: use prerelease tag as channel for console assets cdn (#3975)
- docs: fix broken link (#4005)
- docs: update connecting actions page title (#4008)
- update actions docs (#4007)
- fix syntax error in codeowners file (#4006)
- cli: fix flags in actions, migrate and metadata cmd (fix #3982) (#3991)
- cli(actions): preserve action definition in metadata apply (fix #3988) (#3993)
- build: rename file to adhere to windows rules (close #4002) (#4003)
- translations(readme): add turkish (#3921)
- docs: add AuthGuardian JWT guide (#3958)
- revert auth heading changes in docs (#3992)
- add changelog file to the repo and update pr template (#3946)
- fix docs 404 (#3979)
- tag release v1.2.0-beta.1 (#3966)
- noop: replace subdomain links with subpath (#3869)
- docs: add reference to QualifiedTable to table args (#3880)
- update actions docs (#3953)
- cli: bug fixes related to actions (#3951)
- update docs (#3947)
- fix regression on tag release script (#3944)
- cli: misc fixes related to actions (#3942)
- rfc: check condition in update permissions (#3750)
- docs: add actions docs (#3907)
- cli: allow managing actions (#3859)
- Maintain downgrade commands in a text file, update when tagging (#3933)
- fix new release notification logic (#3930)
- Update Init.hs for newer tags (#3931)
- adds postgres password to docker-compose setup (fix #3894) (#3895)
- update manifests to v1.1.0 (#3913)
- docs: remove wip actions docs (#3909)
- docs: use install manifests from stable branch (#3906)
- console: add actions support (#3889)
- console: show pre-release update notifications with opt out option (#3888)
- translation(readme): add korean (#3818)
- allow custom mutations through actions (#3042)
- run default tests in test_server_upgrade (#3718)
- Add check expresion to update permissions (close #384) (#3804)
- cli: update installation instructions in the readme (fix #3875) (#3876)
- handle invalid keys in permission builder (close #3848) (#3863)
- fix casting citext column type (fix #2818) (#3861)
- Add downgrade command (close #1156) (#3760)
- persist mix files only when coverage is enabled (#3844)
- add meta descriptions to actions docs (#4082)
- `HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL` changes semantics slightly: we only sleep for the interval
  when there were previously no events to process. Potential space leak fixed. (#3839)
- console: track runtime errors (#4083)
- auto-include `__typename` field in custom types' objects (fix #4063)
- squash some potential space leaks (#3937)
- docs: bump MarupSafe version (#4102)
- server: validate action webhook response to conform to action output type (fix #3977)
- server: preserve cookie headers from sync action webhook (close #4021)
- server: add 'ID' to default scalars in custom types (fix #4061)
- server: improve performance of replace_metadata tracking many tables (fix #3802)
- console: add design system base components (#3866)
- docs: add docs for redeliver_event API