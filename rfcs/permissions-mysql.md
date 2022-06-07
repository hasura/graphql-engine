# Table Permissions in MySQL

## Metadata
```
---
authors: Philip Lykke Carlsen <philip@hasura.io>
discussion:
  https://github.com/hasura/graphql-engine-mono/pull/2183
state: published
---
```

## Description

We want to support the [role-based access control
feature](https://hasura.io/docs/latest/graphql/core/auth/authorization/index.html)
on MySQL in the same fasihion that it works on Postgres currently.

The role-based access control feature, often referred to simply as "Permissions"
allows Hasura users to restrict what data is returned by queries and admitted by
mutations. Several flavors of permissions exist:

**_Column Permissions_** censor the columns that cliens in a given role have access
to (either in Queries or Mutations), by means of an explicit list of columns
exposed.

**_Row Permissions_** censor table rows returned or affected, on the basis of a
boolean-returning SQL expression, which is allowed to reference the columns of
the table as well as session variables. These are sometimes also known as
_Filter Permissions_ in the server code. Both _Queries_ and _Mutations_ may be
subjected to _Row Permissions_.

The query semantics of _Column Permissions_ and _Row Permissions_ is that,
in the context of a role:

> _For all intents and purposes, the dataset that a query ranges over includes
  only those columns and rows that the **Column Permissions** and **Row
  Permissions** of that role allow._

For example:
* A user is unable to delete rows that her role's delete-permissions do not
  include. It is not an error to try; from the point of view of the delete
  mutation the rows indicated by the query are just not there.
* A query for an aggregation (say, the average `age` in a `persons` table) will
  only consider those rows that her role's select-permissions include.

The **_Aggregation Permission_** (a single boolean) decides if the query root
fields that relate to aggregations should appear in the schema.

**_Limit Permissions_** (an integer) apply only to queries and limit the
maximum number of rows any query may yield. Importantly, an active _Limit
Permission_ does not influence the row domain of an aggregation query, only the
maximum number of rows produced by the query, see <a name="footnote-1-ref"></a>[[1]](#footnote-1-def).

Last, **_Inherited Roles_** may compose permissions, see [User
docs](https://hasura.io/docs/latest/graphql/core/auth/authorization/inherited-roles.html#select-permissions).
An _Inherited Role_ is an authorization role defined in terms of other
pre-existing roles. The permissions that an _Inherited Role_ grants are _not_
just the point-wise union of each of the parent roles' permission syntax, but
rather the _union of the query datasets_ that each parent role permits, in the
sense described above.

This introduces a complication: In isolation, a role's _Column Permissions_ and
_Row Permissions_ describe a "rectangular" dataset, with columns along one side
and rows along the other. For two or more roles however, when we union the
datasets they permit we do not necessarily end up with a rectangular dataset:

![Inherited roles diagram](permissions-mysql/Inherited%20roles%20permissions.png)

Our data universe however only permits "rectangular" data. In order to
accomodate the complexity resulting from _Inherited Roles_ we make columns that
are particular to a single parent role nullable. For example, in the diagram
above we would return `null` for (`Row 5`, `Column B`) and (`Row 2`, `Column
D`).

## What does this concretely look like

When this is implemented, it should be possible to set permissions on MySQL
tables in exactly the same fashion as is possible on Postgres tables, and
queries and mutations should respect those permissions.

At the time of this writing, this means every tracked MySQL tables has a
_Permissions_ tab in the Console, which allows a user to set permissions on:

* Rows and Columns
  * For each of the CRUD actions
  * Using all the predicates supported as boolean operators in `_where: {..}`
    arguments in queries to MySQL tables.
* Limit and Aggregation

The tests in
[`server/tests-py/test_graphql_queries.py#L575`](https://github.com/hasura/graphql-engine-mono/blob/dfba245a4dbe1a71b1e3cc7c92914fc0a919c2b0/server/tests-py/test_graphql_queries.py#L575)
pertaining to permissions should be generalised to multiple backends and made to
pass for MySQL.

## How are we going to implement it

The GraphQL-Engine applies permissions at three points of processing:

1. When building the schema, where _Column Permissions_ may cause fields to be
   censored from the schema.
2. When parsing an incoming GraphQL query into HGE IR, where _Column Permissions_
   again influence the grammar parsed, and _Row Permissions_ influence the IR
   generated such that relevant permissions are included.
3. When SQL is generated from the IR, where the translation needs to take the IR
   node fields containing permissions into account.

Since parser/schema generation is a single unified abstraction in
GraphQL-Engine, all a backend needs to do to support permissions is a suitable
implementation of type class methods `MonadSchema.buildTableQueryAndSubscriptionFields`,
`buildTableInsertMutationFields` etc..

`buildTableQueryAndSubscriptionFields` et al. are given as inputs a representation of the
permissions for a table (in the context of some role), which for _Column
Permissions_ list the exposed columns and for _Row Permissions_ contain
backend-specific Boolean Expression IR fragments, which are supposed to end up
in parser outputs.

There are already backend-generic implementations of these methods in
`Hasura.GraphQL.Schema.Build` which we may use unless a product requirement
surfaces that require us to deviate from the (de facto) standard table schema.

The inputting and storing of permissions in metadata is handled completely
generically by the core infrastructure referencing only the backend-defined
notions of column names and boolean expressions and how to (de-)serialize them.
The only work that is required here is to expose API endpoints for the various
CRUD-actions on permissions.

### Development plan for Queries

1. Implement the `instance MonadSchema 'MySQL` using the backend-generic default
   implementations.

2. Enabling the API for manipulating permissions amounts to is adding
   `tablePermissionsCommands @MySQL` to the `metadataV1CommandParsers`
   implementation of the `BackendAPI MySQL` instance.

3. For SQL generation of a _Query_, the case that translates an `AnnSelectG`
   <a name="footnote-2-ref"></a>[[2]](#footnote-2-def). Any applicable _Row
   Permissions_ and _Limit Permissions_ are found in
   the field `_asnPerm` and need to translate into `WHERE` and `LIMIT` clauses
   respectably.

4. Also for SQL generation of a _Query_, the case that translates an
   `AnnColumnField` <a name="footnote-3-ref"></a>[[3]](#footnote-3-def)
   needs to observe the field `_acfCaseBoolExpression`, which decides
   whether the column value should be nullified, as resulting from
   inherited roles.

### Notes for Mutations

A GQL _Mutation_ however may result in either of `INSERT`, `UPDATE`, or `DELETE`
statements. Of these, `INSERT` has no obvious point in which to include a permissions
predicate over the rows inserted.

As a consequence of this we need to translate an insert-mutation into a MySQL
transaction, where performing the mutation and checking permissions on the
affected rows is split over multiple statements. <a name="footnote-4-ref"></a>[[4]](#footnote-4-def)

One suggested way to do this could be making a temporary table having the
permissions as `CHECK`-constraints, inserting the new rows into this table
(which fails if the permissions are not satisfied) and copying them over to the
table actually targeted by the mutation.

## Future

This document is a product of its time, brought into existence by the
contemporary need to elaborate on how permissons work because the development
work on MySQL needs to incorporate them.

An insight resulting from discussing this subject is that it would be more
appropriate to treat permissions not as a distinct topic of a dedicated RFC
document, but rather as associated concepts of the RFCs of the objects they
apply to, i.e. variants of _Queries_ and _Mutations_.

As it were, _permissions_ do not exist in a vacuum. In order to talk about them
we need to also talk about what they apply to. As such it makes for a more
elegant exposition to talk about permissons as associated aspects of the subject
they act on.

It it therefore expected that this document be superseded by dedicated RFCs on
the subjects of _Queries_, _Mutations_.

## Questions

How does the feature of _Inherited Roles_ interact with the permissions-support
in a backend?

> The permissions that result from Inherited Roles are completely resolved into
> base permissions before being handed over to schema building. So Inherited
> Roles have no interaction with backend code.

Do _Limit Permissions_ only apply to root-fields or also to array relationships?

> Yet unanswered.

## Footnotes

<a name="footnote-1-def"></a>[1][^](#footnote-1-ref): For example,

```
query {
  articles_aggregate {
    count
    nodes { .. }
  }
}
```

If the select permission on some role `r` specifies a limit of `5` and there are
a total of `10` rows accessible to `r` (as per active _Row Permissions_), the
`count` in the above query should return `10` while `nodes` should only return
`5`. I.e, the _Limit Permission_ should only be applied when returning rows and
not when computing aggregate data.

<a name="footnote-2-def"></a>[2][^](#footnote-2-ref): See [`server/src-lib/Hasura/RQL/IR/Select.hs`]( https://github.com/hasura/graphql-engine-mono/blob/dfba245a4dbe1a71b1e3cc7c92914fc0a919c2b0/server/src-lib/Hasura/RQL/IR/Select.hs#L67).

<a name="footnote-3-def"></a>[3][^](#footnote-3-ref): See [`server/src-lib/Hasura/RQL/IR/Select.hs`]( https://github.com/hasura/graphql-engine-mono/blob/dfba245a4dbe1a71b1e3cc7c92914fc0a919c2b0/server/src-lib/Hasura/RQL/IR/Select.hs#L305). Haddocks contain descriptions of use.

<a name="footnote-4-def"></a>[4][^](#footnote-4-ref): In PostgreSQL we exploit that `INSERT` supports a
`RETURNING` clause that lets us extract information from the affected rows.
MySQL does not support this.
