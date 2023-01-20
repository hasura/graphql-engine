# Roles and Permissions

## What Is a Role?

A role is a label for a group of users, such as `admin`, `editor`, or even
`anonymous`. Every API call from every `graphql-engine` user will be executed
in the context of one of the defined roles, and each role can have some number
of defined _permissions_.

## What Is a Permission?

A permission defines whether or not a particular part of the schema or data is
accessible by the given role. For example, regular users may not be able to
delete other users' blog posts, or we might want to hide certain results from
users who aren't logged in. They may also only be able to select up to a given
number of rows. These things are achieved through the two types of permission:

### Schema Permissions

Schema permissions determine which parts of the GraphQL API are available to
the given role[^1]. This category includes things like column permissions,
aggregation function permissions, and even table permissions[^2]. We refer to
these things as schema permissions because they __change the schema__ for
their role. If a user makes a request for information from a column they can't
see, it's a compile error: the field doesn't exist in the schema.

### Data Permissions

Data permissions do not change the schema, but filter the data that roles are
allowed to interact with. This category includes row permissions and
insert/update "check" permissions (see below). If a user makes a request for
information from a row they can't see, it's not a syntax error, but no results
will be returned.

This is an important distinction to make. If a role has no permissions to see
any columns in a given table, the table won't exist in the schema. If a role
has no permissions to see any _rows_ in a given table (i.e. the row permission
check is always false), the table will be visible, but all queries/mutations
result in empty responses and no changes.

## The Architecture of Permissions

### Schema Permissions

When we combine metadata and database introspection together to build a
[`Hasura.RQL.Types.SchemaCache`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-SchemaCache.html#t:SchemaCache),
each table in the [`SourceCache`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-Source.html#t:SourceCache)
contains a map from roles to their permissions
(a [`RolePermInfoMap`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-Table.html#t:RolePermInfoMap)).
At this point, no distinction is made between schema and data permissions[^3].

Part of building the eventual `SchemaCache` is building role **contexts**. The
context in this case contains parsers for queries, mutations, and
subscriptions.
In [`Hasura.GraphQL.Schema.Action.actionExecute`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Schema-Action.html#v:actionExecute),
any field whose permitted roles do not include the current role will be discarded.
This is where schema permissions are applied: if the schema parser can only parse
fields we are allowed to see, then it follows that we can't see the fields we're not allowed to see.

At this point, each defined role has a context, and each context has a schema
parser. We're ready to expose an API!

### Data Permissions

With that out the way, permissions show up again when we start making actual
GraphQL requests. When a request is made and processed, we compile a request
down to a representation between GQL and (most likely, though not necessarily)
SQL. We call this our intermediate representation, or IR (defined under
`Hasura.RQL.IR`).

The IR defines a collection of types for describing the various actions we
might want to perform as part of a request.
[`Hasura.RQL.IR.Select.AnnSelectG`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-IR-Select.html#t:AnnSelectG),
for example, describes something like a `SELECT` query that we could perform on
a given backend. Within this type, as well as the things we might expect (which
fields do we want, which table are we querying, what predicates must hold), we
also find [`TablePermG`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-IR-Select.html#t:TablePermG):
a description of permissions as a boolean expression.

This is the important point to bear in mind with data permissions such as
row-level permissions: they are implemented as filters that will be placed in
the `WHERE` clause of each query. Following the flow of code for `AnnSelectG`,
we arrive at, for example,
[`Hasura.Backends.Postgres.Translate.Select`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Backends-Postgres-Translate-Select.html),
specifically [`.Internal.Process.processSelectParams`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Backends-Postgres-Translate-Select-Internal-Process.html#v:processSelectParams).
In here, we find (at time of writing) the following block of code:

```haskell
finalWhere =
  toSQLBoolExp (selectFromToQual selectFrom) $
    maybe permFilter (andAnnBoolExps permFilter) whereM
```

`Maybe` we have some `WHERE` clause. If we do, we'll `AND` it with the
permissions filter predicates. If we don't, we'll just use the permissions
filter predicates. This will be done for every query, and this is how row-level
permissions (and data permissions in general) are implemented.

### Boolean predicates

Data permissions are defined (according to `TablePermG`) as an
[`AnnBoolExp`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-IR-BoolExp.html#t:AnnBoolExp):
a boolean algebra, whose core is shared across all backends. While we (currently)
consider [`AND`/`OR`/`NOT`/`EXISTS`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-IR-BoolExp.html#t:GBoolExp)
to be backend-agnostic, backends may vary in
other ways, such as whether aggregation predicates are available. 

This is important when we think about a permission like, "you can only update
this blog post if you created it". In practice, how we achieve this varies
between backends: for example, with Postgres, we implement this by passing the
session variables as a prepared argument (notably the first one), and then
referring to them with `$1`[^4]. For other backends such as MSSQL, we may just
inline the values.

## When do data permissions get applied?

### `SELECT`

As discussed, `SELECT` permissions are added to the final result set as a
filter in the `WHERE` clause. In other words, rows will only be returned who
match _both_ the user's query _and_ the permission predicate. The convention in
the code is to refer to these permissions as a `filter`.

### `INSERT`

We specify permissions for `INSERT` against every row we create. In other
words, we only commit the transaction to insert rows if they all pass the given
predicate. The convention in code is to refer to these permissions as a
`check`.

Note that some `INSERT` queries may _also_ involve `UPDATE` permissions, for
example in the case of `INSERT ON CONFLICT`. In these cases, we use `INSERT`
permissions for created rows, and `UPDATE` permissions for modified rows. If a
user has _no_ permissions to perform an update, for example, `on_conflict` will
be removed from the schema entirely.

### `UPDATE`

These operations involve two sets of permissions: a `filter` on the rows we can
update, and a `check` on the rows that have been updated. These are, in effect,
pre- and post-conditions.

### `DELETE`

Just like `SELECT`, we require only a `filter` precondition to determine
data permissions for deletions.

---

[^1]: Note that `admin` is a little special here: if an `admin` role has no
  permissions, the default is that they can do anything. If another role has no
  permissions, the default is that they can do nothing.

[^2]: This is sort of an extension of column permissions: if a role has no
  permissions to view a given table's columns or aggregates, then that table is
  removed from the schema.

[^3]: Note that the `SchemaCache` is built in two phases: once we have resolved
  the metadata and introspection, we have a **partial** schema cache. We can
  then build the GraphQL schema, while simultaneously building the parsers, and
  the output is a complete schema cache. However, both are represented in code
  by the same `SchemaCache` type.

[^4]: [`Hasura.Backends.Postgres.Execute.Prepare.prepareWithPlan`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-Backends-Postgres-Execute-Prepare.html#v:prepareWithPlan) 
  has some nice documentation on how this is achieved.
