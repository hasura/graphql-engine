# Step 5: Defining the schema

Last but not least we need to give an implementation for `BackendSchema`, which
provides the introspection schema of the queries that our backend supports, as
well as how to build the backend-specific IR from incoming GraphQL queries.

This is a highly technical and somewhat under-abstracted area of the codebase,
so we'll be going through a sample implementation rather than try to develop
one live.

[Hasura.Backends.SQLite.Instances.Schema](../server/src-lib/Hasura/Backends/SQLite/Instances/Schema.hs)


