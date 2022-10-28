# Executing remote joins

When a request has been parsed, and is ready to be executed, we start by
building a `JoinTree`: a structure close to a [prefix
tree](https://en.wikipedia.org/wiki/Trie), containing all the paths in the
response that will require remote joins. We call this phase the
[collection](#collection) phase: it constructs the build tree, and transforms
the request as needed.

After executing the core step of the request, if there is indeed a join tree,
then we start the [join](#join) phase: we fold that tree, expending the response
with the result of each subsequent request.

### Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can generate a default version of it with `M-x
markdown-toc-refresh-toc` (provided by the package markdown-toc), and then edit
it for readability.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [High-level overview](#high-level-overview)
    - [Metadata](#metadata)
    - [Schema cache](#schema-cache)
    - [Building the GraphQL schema](#building-the-graphql-schema)
    - [IR](#ir)
    - [Execution](#execution)
- [The join tree](#the-join-tree)
- [Collect](#collect)
- [Join](#join)
- [Ambiguous schemas](#ambiguous-schemas)

<!-- markdown-toc end -->

## High-level overview

Remote relationships are a complex feature, that touches a lot of different parts of the
code. This section breaks down where each main moving part is located.

### Metadata

Types that represent remote relationships in the metadata are, at time of
writing, located in
[`RQL.Types.Relationships.Remote`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-Relationships-Remote.html). We
support two different formats in the metadata, for historical reasons: the "old
style" is accepted for relationships from sources to remote schemas, which
originally was the only kind of relationships we supported.

Some sub-components of the metadata are defined in separate folders, in an
effort to modularize our codebase; for instance, while the aforementioned module
contains all top-level definitions of remote relationships, the specifics of how
to join against a remote schema are defined in
[`RemoteSchema.Metadata.RemoteRelationship`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RemoteSchema-Metadata-RemoteRelationship.html).

### Schema cache

When translating from the metadata to the schema cache, we resolve remote
relationships alongside the object on their left-hand side; for instance, when
processing tables, we translate remote relationships as corresponding fields in
the resulting `TableInfo`.

The types that make up the translated ("resolved") relationship information are
also located in
[`RQL.Types.Relationships.Remote`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-Types-Relationships-Remote.html), and the code that does the translation is split between `RQL.DDL.Schema.Cache` and other surrounding files. But, likewise, specific parts are being moved out, and some of the code now resides in [`RemoteSchema.SchemaCache.RemoteRelationship`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RemoteSchema-SchemaCache-RemoteRelationship.html).

### Building the GraphQL schema

The GraphQL schema is built in a depth-first fashion (see [this page](schema.md)
for more info). Consequently, whenever we encounter a remote relationship we
traverse it and start building the relevant sub-section of the schema on the
right-hand side. This creates a problem: it ties the LHS and the RHS of the
relationship together, as the schema-building code of the former needs to know
how to build the schema of the RHS. To avoid this, we do something akin to
"dependency injection": we encapsulate the details of the RHS in a function
that, given a relationship, can build the relevant part of the schema, and we
use that function throughout the schema.

That function is
[`remoteRelationshipField`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Schema-RemoteRelationship.html#v:remoteRelationshipField);
it is made available to the schema code as part of the
[`SchemaContext`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Schema-Common.html#t:SchemaContext),
as a
[`RemoteRelationshipParserBuilder`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Schema-Common.html#t:RemoteRelationshipParserBuilder). That
context is initialized at the [root of the
schema](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Schema.html).

### IR

After successfully parsing a remote relationship, it needs to be represented in
the IR. For that purpose, we mostly reuse [schema cache
types](#schema-cache). We also have a few additional types encapsulating remote
relationships as a whole, such as
[`RemoteRelationshipField`](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-RQL-IR-Root.html#t:RemoteRelationshipField).

### Execution

The rest of this document is about the actual execution of remote joins, as part
of the broader execution of a query. Most of the corresponding code is located
in `Hasura.GraphQL.Execute.RemoteJoin`.

## The join tree

As mentioned, the join tree is almost like a prefix tree; the key difference is
that we don't store values at arbitrary points of the tree, only at the
leaves. Furthermore, while most prefix trees are indexed by character, in our
case we index joins by the *path through the response*.

For instance, imagine that we send the following request:

```graphql
query {
  authors {
    name
    articles { # remote join
      title
    }
  }
}
```

the join tree we would emit would have the following shape:

```yaml
(Nothing, authors):
  (Nothing, articles): <join information>
```

Recursively, all the way down, each join information might contain its own join
tree if there are any nested remote relationship.

Each key in this join tree is a pair: it contains the name of the field, but
also contains an optional type information: this is used to deal with [ambiguous
schemas](#ambiguous-schemas).

## Collect

Implemented in
[Hasura.GraphQL.Execute.RemoteJoin.Collect](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Execute-RemoteJoin-Collect.html),
this phase identifies the remote joins in a request, and transforms the request
accordingly. If a selection set contains a field that is a remote join, we alter
the selection set:
  - the field that maps to a remote join is replaced by a placeholder value, so
    that we can keep track of the order in the selection set (since that order
    must not be altered)
  - we add "phantom fields": fields that were not requested by the user, but
    that we need to include, as they are the keys on which the join is performed

A difficulty here concerns those phantom fields: every join requires the "join
keys", the fields on the left-hand side, which means we need to ensure that they
are present in the first request, but removed from the final result. However,
some of the fields we need to fetch might already be part of the request,
perhaps under a different name: to avoid fetching the same field multiple times,
some complicated duplication must first happen. Internally, we keep track of a
"joins key map", that, to a given join key, associates the name of the field we
need to extract from the answer.

In the case where the request goes to a remote schema, we might need additional
transformations (see the section on [ambiguous schemas](#ambiguous-schemas)).

From a practical perspective, the collection is a glorified `traverse`,
operating in the `Collector` monad, which itself is a `Writer` monad: whenever
we encounter a remote join, we `tell` it to the collector, and continue our
traversal. Every time we traverse a field, we use `censor` to wrap the resulting
joins in a sub-tree. Remote joins are aggregated using the `Semigroup` instance
of `JoinTree`.

## Join

Implemented in
[Hasura.GraphQL.Execute.RemoteJoin.Join](https://hasura.github.io/graphql-engine/server/haddock/main/Hasura-GraphQL-Execute-RemoteJoin-Join.html),
we post-process the root request by "folding" the tree of joins: we traverse the
join tree alongside the response: for each field in the response that maps to a
leaf of the join tree, we recursively do the same thing: issue a query, traverse
its own join tree... and on the way back, we replace the value of field by the
result of the join.

Depending on whether the target is a remote schema or a local source, we call
either `makeRemoteSchemaJoinCall` or `makeSourceJoinCall`, defined in
[Hasura.GraphQL.Execute.RemoteJoin.RemoteServer](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/RemoteJoin/RemoteServer.hs)
and
[Hasura.GraphQL.Execute.RemoteJoin.Source](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/RemoteJoin/Source.hs)
respectively.

More specifically, for each remote join we encounter while traversing the join
tree, we use its internal "join keys map" to extract all the join values
required to perform the join. We **only perform the join if all of them are
non-null**. We treat a null value as a missing value, that cannot be used as a
join key.

## Ambiguous schemas

This process is made more complicated by the fact that remote schemas, via
unions and interfaces, can be ambiguous. Consider the following request:

```graphql
query {
  node(id: $some_id) {
    ... on Article {
      # foo is a field, returns data of type `t`
      foo {
         # r1 is a REMOTE relationship, returns data of type `u`
         bar: r1 {
         }
      }
    }
    ... on Author {
      id
      # foo is a field, returns data of type `t`
      foo {
         # r2 is a REMOTE relationship, returns data of type `u`
         bar: r2 {
         }
      }
    }
  }
}
```

There are several complications with this request:
  - there are two remote joins that would need to be at the same point in the
    join tree, `node.foo.bar`;
  - we need to identify which of the two relationships it is when processing the
    joins; but we can't do so using information about `foo`, since its
    `__typename` will be `t` in both cases.

To fix this, we have altered the join tree: instead of using the field name as
key at each level, we use instead a combination of optional type name and field
name. We identify as "ambiguous" all selection sets of a union or an interface
that either directly contain remote joins, or whose subselections contain remote
joins. Whenever we encounter such a selection set, we use its type name in the
corresponding keys in the join tree, and we add one more phantom field to the
selection set: `__hasura_internal_typename`, which extracts the `__typename`.

When processing the joins, we look for the presence of this field: if it is
there, we remove it from the response, and we do the join tree lookup using its
value, instead of using `Nothing`.

In practice, the join tree for the aforementioned query would therefore be:
```yaml
(Nothing, node):
  (Article, foo):
    (Nothing, bar): <join info>
  (Author, foo):
    (Nothing, bar): <join info>
```
