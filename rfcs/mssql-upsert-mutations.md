# Upserts on SQL Server

- [Upserts on SQL Server](#upserts-on-sql-server)
  - [Metadata](#metadata)
  - [User story](#user-story)
  - [API](#api)
    - [Request](#request)
      - [insert (upsert) syntax](#insert-upsert-syntax)
      - [`if_matched` argument](#if_matched-argument)
      - [example](#example)
    - [Response](#response)
  - [Success](#success)
  - [Checkpoints](#checkpoints)
  - [Design](#design)
    - [1. Support backend-agnostic column mutability](#1-support-backend-agnostic-column-mutability)
    - [2. Reconsider, refactor or rename `XOnConflict` and `ExtraInsertData`](#2-reconsider-refactor-or-rename-xonconflict-and-extrainsertdata)
    - [3. Generate `upsert` mutation schema](#3-generate-upsert-mutation-schema)
    - [4. SQL generation & execution](#4-sql-generation--execution)
  - [Other info](#other-info)
  - [Future Work](#future-work)

## Metadata
authors: [Abby](http://github.com/sassela), [Vamshi](https://github.com/0x777)

Teams Involved: [Data Sources](https://github.com/orgs/hasura/teams/server-data-sources), [Docs](https://github.com/orgs/hasura/teams/hge-docs-owners) and [Console](https://github.com/orgs/hasura/teams/hge-console-owners)

## User story

As a user, I would like to be able to upsert rows in from a certain mssql table using a predicate, similarly to how I'm able to do so for a postgres table.

Upserting rows into a table should be done via a GraphQL mutation to the `/v1/graphql` endpoint.

Upserts should respect:
- [row-level permissions](https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#row-level-permissions)
- [column-level permissions](https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#col-level-permissions)
- [role-based column presets](https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#col-presets-permissions)


## API

### Request

<!-- Request and response from the users' perspective -->
#### insert (upsert) syntax

```
mutation [<mutation-name>] {
    <mutation-field-name> (
        [<input-object>!]
        [<matched-clause>]
    )
    [<mutation-response>!]
}
```

| Key                 | Required | Schema                                                                                                                     | Description                                                                  |
| ------------------- | -------- | -------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `matched-clause`    | `false`  | [`if_matched`](#if_matched-argument)                                                                                       | columns that are allowed to be selected, this determines the 'ON' expression |
| `input-object`      | `true`   | [`objects`](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#inputobjects)               | same as postgres. columns that can should be updated when  matched           |
| `mutation-response` | `true`   | [`mutation response`](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#mutationresponse) | same as postgres                                                             |

#### `if_matched` argument
The `if_matched` clause is used to convert an insert mutation to an upsert mutation, similar to Postgres' `on_conflict` clause. Upsert respects the table’s update permissions before editing an existing row in case of a match. Hence the `if_matched` clause is permitted only if a table has update permissions defined.

```graphql
if_matched {
    # columns that are allowed to be selected
    # this determines the 'ON' expression
    match_columns: [table_select_column]

    # columns that can should be updated when  matched
    # (same as postgres' update_columns in on_conflict)
    update_columns: [table_update_column]

    # same as postgres, I think we can 'AND' this with
    # the 'ON' condition derived from 'match_columns'
    where: table_bool_exp
}
```

#### example
```graphql
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

Comparable to the [Postgres insert/upsert API](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#insert-upsert-syntax)

### Response

The mutation response is specified in the [GraphQL spec](https://spec.graphql.org/June2018/#sec-Mutation), including:

```graphql
{ data # the returned data is specified by the `mutation-response` section
    {
    affected_rows
    returning { # the `returning` statement can include nested objects
        response-field1
        response-field2
        ..
    }
    }
}
```

...or, in the event of an unsuccessful mutation:

```graphql
{ errors {
    extensions
    message
  }
}
```

[reference](https://hasura.io/docs/latest/graphql/core/api-reference/graphql-api/mutation.html#mutationresponse)

## Success

<!-- How do we know if we've solved this problem?

This could include a specific list of acceptance criteria.

This could outline specific edge cases that need to be handled.

This section should be more high-level, with more detail added in the subsequent **What** section. -->
- [TestGraphqlInsertOnConflict](https://github.com/hasura/graphql-engine-mono/blob/936f138c80d7a928180e6e7b0c4da64ecc1f7ebc/server/tests-py/test_graphql_mutations.py#L115) tests pass for MSSQL, demonstrating:
  - simple upsert: `test_on_conflict_update`
  - upsert with filter: `test_order_on_conflict_where`
  - upsert request ignored: `test_on_conflict_ignore`
  - consider renaming "on conflict" tests to refer to something more backend-agnostic like "upserts".
- [ ] TODO [Vamshi](https://github.com/0x777) will write a description [like this](https://github.com/hasura/graphql-engine-mono/pull/2403#issuecomment-933630333) to retroactively refine the acceptance criteria, and propose a minimal set of tests for upserts to replace the ones above. However, this shouldn't block implementation.
- upserts are executable via the console and CLI.
- users can define row-level and column-level permissions for upserts via the console and CLI
- upserts on SQL Server are documented in Hasura docs. The [existing Postgres docs](https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/upsert.html) can be used as a guide.


## Checkpoints

<!-- Roughly, what does this look like in the product?

Include wireframes and mockups here.

Are there things that we don't yet know yet? Are we currently doing an R&D Spike to evaluate? -->

*These checkpoints do not necessarily need to be delivered in the same PR. In fact, prefer smaller PRs where they are functional, tested, and self-contained.*
- [ ] [Support backend-agnostic column mutability](https://github.com/hasura/graphql-engine-mono/issues/2770)
- [ ] Reconsider, refactor or rename `XOnConflict` and `ExtraInsertData`
- [ ] Generate `upsert` mutation schema
- [ ] Query translation / execution

## Design

### 1. Support backend-agnostic column mutability

GraphQL Engine's current mutations implementation relies on a Postgres-specific concept of column identity, which doesn't adequately describe the conditions in which columns can be mutated in SQL Server, nor other databases we want to support in future.

[This RFC](https://github.com/hasura/graphql-engine-mono/blob/main/rfcs/column-mutability.md) proposes a backend-agnostic concept of column mutability, which we'll need in order to support upserts across SQL Server and alternative backends.

[Supporting backend-agnostic column mutability](https://github.com/hasura/graphql-engine-mono/issues/2770) is a prerequisite to supporting upserts on SQL Server.

### 2. Reconsider, refactor or rename `XOnConflict` and `ExtraInsertData`
`ExtraInsertData` and `ExtraTableMetadata` both seem to encode backend-specific information about what's needed to mutate columns.

`XOnConflict` which AFAICT is a type-level flag for the `on-conflict` field parser.

*Open questions:* In light of the column mutability work that needs to be done anyway, I think we should take the opportunity to discuss in this PR:
1. whether `ExtraInsertData`, `ExtraTableMetadata` and `XOnConflict` would still be necessary after the above changes.
2. if so, whether any can be refactored or renamed to make their meaning clearer.

In particular, there were some valid points of feedback in the [insert mutation PR](https://github.com/hasura/graphql-engine-mono/pull/2248) that seem unaddressed. Regardless of whether the feedback was missed or resolved, I think now is a good time to reconsider:
- [ ] [do we still need `XOnConflict`?](https://github.com/hasura/graphql-engine-mono/pull/2248#discussion_r710982823)
  - [ ] if so, could we rename it to something more generic like `XUpserts`?
- [ ] do we still need `ExtraInsertData`? [[1]](https://github.com/hasura/graphql-engine-mono/pull/2248#discussion_r710981216)[[2]](https://github.com/hasura/graphql-engine-mono/pull/2248#pullrequestreview-762404235)
  - [ ] [if so, can we more clearly model the behaviour of `getExtraInsertData` in the absence of PKs?](https://github.com/hasura/graphql-engine-mono/pull/2248#discussion_r718311871)


```
+----------+     +----+     +--------------+     +-----+     +-----------------+
| Metadata | --> | IR | --> | Mutation AST | --> | SQL | --> | Execution on DB |
+----------+     +----+     +--------------+     +-----+     +-----------------+
```
### 3. Generate `upsert` mutation schema
```
+----------+     +----+
| Metadata | --> | IR |
+----------+     +----+
```
Generate schema for insert mutations with an `if_matched` clause, with permissions enforced. This broadly involves:

1. implementing the `upsert` object parser: implement `defaultConflictObject` for an MSSQL backend. Consider renaming the class method to use a more generic term like "upsert", e.g. `upsertObject`.

1. introducing a generic field parser for `upsert` arguments: introduce a new method on the `BackendSchema` typeclass to parse the upsert argument of an insert field for a given backend. For example:
    ```haskell
      mkUpsertParser ::
          MonadParse m =>
          Maybe (Parser 'Input m a) ->
          InputFieldsParser m (Maybe a)
    ```

1. implementing the field parser for MSSQL: implement `mkUpsertParser` to correctly parse the `if_matched` argument. The parser should return `InputFieldsParser [...] Nothing` when permissions are not met. The existing `mkConflictArg` can be used as a reference implementation.

1. update the `AnnInsert` IR: update `insertIntoTable` and `insertOneIntoTable` (at least) to use the newly introduced `BackendSchema` methods instead of `mkConflictArg`.

1. optionally, update `objectRelationshipInput` and `arrayRelationshipInput` to also use the `BackendSchema` field parser; however, this *may* not be neccesary as nested inserts are not yet supported on SQL Server.

*To verify:* The generated schema can be verified locally in Hasura Console's Documentation Explorer. This change, if successful, should result in the following generated schema diff for an example `author` table:
```diff
type mutation_root {
  insert_author(
    objects: [author_insert_input!]!
+   if_matched: author_if_matched
  ): author_mutation_response

  insert_author_one(
    object: author_insert_input!
+   if_matched: author_if_matched
  ): author
}

+input author_if_matched {
+  match_columns: author_match_columns!
+  update_columns: [author_update_column!]! = []
+  where: author_bool_exp
+}
```

The mutation, if attempted, won't succeed until the next step is implemented.

```
+--------------+     +-----+     +-----------------+
| Mutation AST | --> | SQL | --> | Execution on DB |
+--------------+     +-----+     +-----------------+
```

### 4. SQL generation & execution
This broadly involves:
1. creating an intermediate mutation AST: for example, `data Merge` in `Hasura.Backends.MSSQL.Types`.
     - [These notes on MSSQL upsert](https://github.com/hasura/graphql-engine-mono/issues/2427) give an example `MERGE` statement in context of HGE upserts.
     - Postgres' `SQLConflict` datatype can be used as a reference implementation.
1. using AST: including this new type as a field in MSSQL's `Insert` record
1. translating AST to a [SQL Server `MERGE` statement](https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql): update the `fromInsert` query printer to generate the correct `MERGE` statement when the upsert condition is met.

*To verify:* simple insert tests such as `test_on_conflict_update`, at a minimum, should now pass.

## Other info
- [SQL Server upsert API design notes](https://github.com/hasura/graphql-engine-mono/issues/2427)
- [SQL Server `MERGE` docs](https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql)

## Future Work
- [ ] Is there anything specific we need to consider for upserts related to...
  - Nested inserts?
  - Mutations for tables without primary keys?
