# RFC: Mutations for MSSQL Server

### Metadata

Authors: [Rakesh](http://github.com/rakeshkky)

State: Draft

Teams Involved: [Data Sources](https://github.com/orgs/hasura/teams/server-data-sources), [Docs](https://github.com/orgs/hasura/teams/hge-docs-owners) and [Console](https://github.com/orgs/hasura/teams/hge-console-owners)

Slack Channels: #team-data-sources, #feature-ms-sql

Other Docs:
- [Postgres Mutations](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#graphql-api-mutation)
- [MSSQL Transactions](https://docs.microsoft.com/en-us/sql/t-sql/language-elements/transactions-transact-sql?view=sql-server-ver15)
- [MSSQL Insert Syntax](https://docs.microsoft.com/en-us/sql/t-sql/statements/insert-transact-sql?view=sql-server-ver15)

### Description

Generation and execution of GraphQL insert, delete and update mutations for MSSQL backend with permissions enforced.

### Success criteria

Taking reference to [Postgres mutations](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#graphql-api-mutation)
we should able to generate schema and execute mutations for MSSQL backend.

For example, let's say a table with name `author` is tracked from a MSSQL server backend. Considering insert
mutations,

**Schema Generation**:
The server should able to generate following GraphQL schema

```graphql

type mutation_root {
    insert_author(objects: [author_insert_input!]!): author_mutation_response
}

input author_insert_input{
    id: Int!
    name: String!
}

type author_mutation_response {
    affected_rows: Int!
    returning: [author!]
}
```

**Query Execution**:
The server should able to execute following sample GraphQL mutation

```graphql

mutation {
    insert_author(objects: [{name: "Bob"}]){
        affected_rows
        returning{
            id
            name
        }
    }
}
```

**Permissions**:
Users should able to define row-level and column-level permissions for inserts via Metadata API
or Console UI

### How

```
+----------+  Schema Parser generation   +----+  Translation   +-----+  With transactions   +-----------------+
| Metadata | --------------------------> | IR | -------------> | SQL | -------------------> | Execution on DB |
+----------+  with permissions enforced  +----+                +-----+                      +-----------------+
```

#### Schema Generation
Generate schema for MSSQL mutations with permissions enforced. Existing logic for Postgres schema generation
applies here with minimal changes applicable to IR abstraction.

#### Query Translation / Execution
```
 +----+                +--------------+              +-----+
 | IR | -------------> | Mutation AST | -----------> | SQL |
 +----+                +--------------+              +-----+
```

Mutation AST is a data type which can be readily translated to SQL Text. We need to have data types for each mutation operation.
- `data Insert` for Insert
- `data Update` for Update
- `data Delete` for Delete

Specimen SQL for reference: Unlike Postgres, we cannot integrate DML statements in [common table expression](https://docs.microsoft.com/en-us/sql/t-sql/queries/with-common-table-expression-transact-sql?view=sql-server-ver15) of MSSQL. Only SELECTs are allowed in a common table expression.
Our proposal is to use [temporary tables](https://www.sqlservertutorial.net/sql-server-basics/sql-server-temporary-tables/) to capture mutated rows and generated appropriate response using SELECT statement.

```mssql
INSERT INTO test (name, age) OUTPUT INSERTED.<column1>, INSERTED.<column2> INTO #temp_table values ('rakesh', 25)

WITH some_alias AS (SELECT * FROM #temp_table)
SELECT (SELECT * FROM  some_alias FOR JSON PATH, INCLUDE_NULL_VALUES)  AS [returning], count(*) AS [affected_rows] FROM some_alias FOR JSON PATH, WITHOUT_ARRAY_WRAPPER;
```

For **tables without primary key**, we choose **not** to generate mutations schema atleast in the initial iterations.


#### Permissions
Like in Postgres, we need to generate expression to evaluate the check condition and return it as an extra field.
If check constraint is not satisfied we'll raise exception in the Haskell code.

```mssql
INSERT INTO test (name, age) OUTPUT INSERTED.<column1>, INSERTED.<column2> INTO #temp_table values ('rakesh', 25)

WITH alias AS (SELECT * FROM #temp_table)
SELECT (SELECT (SELECT * FROM alias FOR JSON PATH, INCLUDE_NULL_VALUES) AS [returning], count(*) AS [affected_rows] FROM alias FOR JSON PATH, WITHOUT_ARRAY_WRAPPER) AS [data], SUM(case when (id = 12) then 0 else 1 end) AS [check_constraint] FROM alias ;

```

#### Haskell interface to execute MSSQL transactions
Mutations are executed safely via database transactions.
We need to have a separate interface/library to implement [MSSQL Transactions](https://docs.microsoft.com/en-us/sql/t-sql/language-elements/transactions-transact-sql?view=sql-server-ver15).
Preferably, the library implementation details should be specified in a separate RFC. Our in-house
written [Postgres' transaction](https://github.com/hasura/pg-client-hs/blob/master/src/Database/PG/Query/Transaction.hs) library acts as good reference.

#### Testing
Generate pytests to test
- Simple mutations with returning affected rows and count
- Mutations involving all stock column types
- Mutations with permissions

Refer to [Postgres mutation tests](https://github.com/hasura/graphql-engine-mono/blob/rfc-nonvolatile-functions-mssql/server/tests-py/test_graphql_mutations.py)

### Future Work

- Upsert feature
- Nested inserts
- Mutations for tables without primary keys
