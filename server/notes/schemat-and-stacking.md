This note is in [Hasura.GraphQL.Schema.Common](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Common.hs#L158).
It is referenced at:
  - line 56 of [Hasura.GraphQL.Schema.RemoteRelationship](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/RemoteRelationship.hs#L56)
  - line 65 of [Hasura.GraphQL.Schema.RemoteRelationship](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/RemoteRelationship.hs#L65)

# SchemaT and stacking

The schema is explicitly built in `SchemaT`, rather than in an arbitrary monad
`m` that happens to have the desired properties (`MonadReader`, `MonadMemoize`,
`MonadError`, and so on). The main reason why we do this is that we want to
avoid a specific performance issue that arises out of two specific constraints:

  - we want to build each part of the schema (such as sources and remote
    schemas) with its own dedicated minimal reader context (i.e. not using a
    shared reader context that is the union of all the information required);
  - we want to be able to process remote-relationships, which means "altering"
    the reader context when jumping from one "part" of the schema to another.

What that means, in practice, is that we have to call `runReaderT` (or an
equivalent) every time we build a part of the schema (at the root level or as
part of a remote relationship) so that the part we build has access to its
context. When processing a remote relationship, the calling code is *already* in
a monad stack that contains a `ReaderT`, since we were processing a given part
of the schema. If we directly call `runReaderT` to process the RHS of the remote
relationship, we implicitly make it so that the monad stack of the LHS is the
base underneath the `ReaderT` of the RHS; in other terms, we stack another
reader on top of the existing monad stack.

As the schema is built in a "depth-first" way, in a complicated schema with a
lot of remote relationships we would end up with several readers stacked upon
one another. A manually run benchmark showed that this could significantly
impact performance in complicated schemas. We do now have a benchmark set to
replicate this specific case (see the "deep_schema" benchmark set for more
information).

To prevent this stacking, we need to be able to "bring back" the result of the
`runReaderT` back into the calling monad, rather than defaulting to having the
calling monad be the base of the reader. The simplest way of doing this is to
enforce that we are always building the schema in a monad stack that has the
reader on top of some arbitrary *shared* base. This gives us the guarantee that
the LHS of any remote relationship, the calling context for `runReaderT`, is
itself a `ReaderT` on top og that known shared base, meaning that after a call
to `runReaderT` on another part of the schema, we can always go back to the
calling monad with a simple `lift`, as demonstrated in
'remoteRelationshipField'.

