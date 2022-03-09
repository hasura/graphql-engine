## Table of contents

<!--
Please make sure you update the table of contents when modifying this file. If
you're using emacs, you can automatically do so using the command mentioned in
the generated comment below (provided by the package markdown-toc), but it will
use a slightly different format and you will have to fix the differences
manually.
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

- [The join tree](#the-join-tree)
- [Collect](#collect)
- [Join](#join)
- [Ambiguous schemas](#ambiguous-schemas)

<!-- markdown-toc end -->

## Executing remote joins

When a request has been parsed, and is ready to be executed, we start by
building a `JoinTree`: a structure close to a [prefix
tree](https://en.wikipedia.org/wiki/Trie), containing all the paths in the
response that will require remote joins. We call this phase the
[collection](#collection) phase: it constructs the build tree, and transforms
the request as needed.

After executing the core step of the request, if there is indeed a join tree,
then we start the [join](#join) phase: we fold that tree, expending the response
with the result of each subsequent request.

### The join tree

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

### Collect

Implemented in
[Hasura.GraphQL.Execute.RemoteJoin.Collect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/RemoteJoin/Collect.hs),
this phase identifies the remote joins in a request, and transforms the request
accordingly. If a selection set contains a field that is a remote join, we alter
the selection set:
  - the field that maps to a remote join is replaced by a placeholder value, so
    that we can keep track of the order in the selection set (since that order
    must not be altered)
  - we add "phantom fields": fields that were not requested by the user, but
    that we need to include, as they are the keys on which the join is performed

In the case where the request goes to a remote schema, we might need additional
transformations (see the section on [ambiguous schemas](#ambiguous-schemas)).

From a practical perspective, the collection is a glorified `traverse`,
operating in the `Collector` monad, which itself is a `Writer` monad: whenever
we encounter a remote join, we `tell` it to the collector, and continue our
traversal. Every time we traverse a field, we use `censor` to wrap the resulting
joins in a sub-tree. Remote joins are aggregated using the `Semigroup` instance
of `JoinTree`.

### Join

Implemented in
[Hasura.GraphQL.Execute.RemoteJoin.Join](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/RemoteJoin/Join.hs),
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

### Ambiguous schemas

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
