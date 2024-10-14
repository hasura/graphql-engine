# Query Usage Analytics

We want to gather usage analytics from a GraphQL query. Usage analytics of
models, commands, fields, permissions, relationships etc. being used in the
query.

For now, this crate defines the types we would construct for gathering usage
analytics. Later, this can also include functions required to build up these
types. Ideally, this crate should expose an API which the engine IR generation
or query plan generation can use to create a usage analytics type and serialize
and emit it.

The types here are a simplified version of the GraphQL query AST, OpenDD and
metadata-resolve types.
