# Hasura GraphQL Engine Changelog

## `v1.2.0` (in beta)

### New features :tada:

### Hasura Actions

Actions are a way to extend Hasura’s auto-generated mutations with entirely custom ones which can handle various use cases such as data validation, data enrichment from external sources and any other complex business logic.

A new mutation can be created either by defining its GraphQL SDL or by deriving it from an existing Hasura-generated mutation. The resolver is exposed to Hasura as a webhook which can be called synchronously or asynchronously. This release also includes an ever evolving codegen workflow to make managing the custom resolvers easier.

Read more about actions in the [docs](https://docs.hasura.io/1.0/graphql/manual/actions/index.html).

(#3042) (#3252) (#3859)

#### Manage Postgres Check Constraints from Console

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

#### Downgrade command

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

#### Check expression in update permissions

wip

(close #384) (rfc #3750) (#3804) 

