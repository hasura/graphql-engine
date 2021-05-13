# Future breaking changes to the GraphQL API

For V2, despite the many internal changes to the engine, we have tried as much as possible to keep a complete backwards compatibility with V1. In several places, this has meant introducing special cases in the code, making a feature more confusing, or delaying improvements. At some point in the future, when we feel that it might be reasonable to push a release that has breaking changes (a future v3 perhaps), we will want to clean those pain points.

The goal of this document is to list all such potential changes. They are in no particular order. Not all of those will necessary be implemented.

## Non-overloaded graphql scalars

At time of writing, the way a given scalar will be parsed *depends on where it is used*. For instance:
- the "limit" argument of a table is declared as an `Int` in the schema but will reject negative values
- the "offset" argument of a table is declared as an `Int` in the schema but will accept string literals to support big ints
- the "int" postgres column type will be declared as an `Int` in the schema but will accept string literals

It'd be preferable if there was a 1:1 mapping between the type as it is declared in the schema and the way it is parsed, to remove all possible ambiguities. We could, for instance:
- expose "limit" as a `NonNegativeInt`
- expose "offset" as a `BigInt`
- expose a postgres int as a `pg_int`

This would be a breaking change, since queries using variables of type `Int` for any of those fields would now be rejected with a type error.

Furthermore, as a side bonus, this could be an opportunity to harmonize different parsers (Postgres column values have several different parsers: from JSON, in the schema... and disagreements between them as been a source of issues.)

## Explicit transaction semantics

V2 brought heterogeneous queries to the engine: it is possible to emit a query or mutation that targets different sources at once:

```graphql
query {
  // stored on Postgres
  purchases(limit: 10) {
    name
    date
  }

  // stored on MSSQL
  invoices(where: {id: {_eq: {42}}}) {
    tag
    value
  }

  // remote server
  finances {
    monthly {
      balance
    }
  }
}
```

But for mutations, this broke transaction semantics: the engine does not support transaction semantics across different sources. To keep compatibility with v1, the [following optimisation](https://github.com/hasura/graphql-engine/commit/71ae144aa623c156064ffe21eb38020ccf315a7a) is performed: if all the root fields in a mutation target the same Postgres source, then transaction semantics are applied, otherwise they're not.

While this keeps backwards compatibility with v1, the downside is that the user has **no control** over the transaction semantics: whether a mutation will be executed as a transaction or not can't be known by just reading the graphql query.

The proposed change would be to introduce *explicit* transaction semantics. The syntax could be, for instance:

```graphql
mutation {
  // all going to the same database
  transaction {
    insert_author_one(object: {id: 101, name: "Mark Z. Danielewski"}) {
      name
    }
    insert_book_one(object: {ISBN: "0-375-70376-4", name: "House of Leaves", author_id: 101}) {
      name
    }
  }
  // can go to a different source, as it's not part of the transaction
  insert_diary_one(object: {entry: "ſtaires! We have found ſtaires!"}) {
    date
    entry
  }
}
```

## Explicit boolean collapse

In v2, we introduced a breaking change, that aimed at fixing a [long-standing issue](https://github.com/hasura/graphql-engine/issues/704): a null value in a boolean expression would always evaluate to `True` for all rows. For example, the following queries were all equivalent:

```graphql
delete_users(where: {_id: {_eq: null}})  # field is null, which is as if it were omitted
delete_users(where: {_id: {}})           # object is empty, evaluates to True for all rows
delete_users(where: {})                  # object is empty, evaluates to True for all rows
delete_users()                           # delete all users
```

This behaviour was unintuitive, and could be an unpleasant surprise for users that expected the first query to mean "delete all users for whom the id column is null". Therefore in v2, we changed the implementation of boolean operators to reject null values, as we deemed it safer:


```graphql
delete_users(where: {_id: {_eq: null}})  # error: argument of _eq cannot be null
```

However, this change broke the workflows of [some of our users](https://github.com/hasura/graphql-engine/issues/6660) who were relying on this property of boolean operators. This was used, for instance, to _conditionally_ enable a test:

```graphql
query($isVerified: Boolean) {
  users(where: {_isVerified: {_eq: $isVerified}}) {
    name
  }
}
```

For users who preferred the old behaviour, we now have an environment variable, `HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE`, that **globally changes the behaviour of all boolean operators**. The proposed breaking change would be to instead introduce new operators with this behaviour and remove the environment variable, so that users can choose explicitly on a per query basis which behaviour they want.

The syntax could be something like:

```graphql
query($isVerified: Bool) {
  # collapses to True if $isVerified is null
  users(where: {verified: {_maybe_eq: $isVerified}}) {
    name
  }
}

mutation($deleteId: Int) {
  # never collapse!
  delete_users(where: {id: {_eq: $deleteId}}) {
    affected_rows
  }
}
```

## New order_by syntax

As evidenced by [#6778](https://github.com/hasura/graphql-engine/issues/6778), the syntax of `order_by` is non-intuitive: we expect a list of objects with only one field each, since GraphQL input types are unordered. For example, the proper syntax for ordering a table of users by age descending THEN by name ascending would be:

```graphql
query {
  users(order_by: [{age: desc}, {name: asc}]) {
    name
    age
  }
}
```

However, due to input coercion rules, and due to how GraphiQL works, it is very easy and tempting to instead write it as such:

```graphql
query {
  users(order_by: {age: desc, name: asc}) {
    name
    age
  }
}
```

But in this case, **there is no guarantee** that the resulting SQL query will be `ORDER BY AGE DESC, NAME ASC`; it will depend on external factors, such as the order of the fields in the table. This used to (accidentally?) work in v1, but is no longer the case in v2.

We are planning to reject such queries, in the future, with a clear error message explaining how to craft the query instead, but there is a potential breaking change that would easily solve this issue: introduce a new enum type that covers all possible cases. The syntax would become:

```graphql
query {
  users(order_by: [age_desc, name_asc]) {
    name
    age
  }
}
```

Without objects, there is no longer an invalid way of representing `order_by`.
