# Step 1: Add source

The connection to a backend source is defined by the associated types `SourceConfig 'SQLite` and
`SourceConnConfiguration 'SQLite` in the `Backend` type class, in [Hasura.Backends.SQLite.Instances.Types](../server/src-lib/Hasura/Backends/SQLite/Instances/Types.hs).

We're going to collect our type definitions in a separate module: [Hasura.Backends.SQLite.Types](../server/src-lib/Hasura/Backends/SQLite/Types.hs).

In order to actually connect to an instance, we fill in `resolveSourceConfig` method
in the `BackendMetadata` type class, in [Hasura.Backends.SQLite.Instances.Metadata](../server/src-lib/Hasura/Backends/SQLite/Instances/Metadata.hs).

Next up is [Step 2: SQLite metadata](2-metadata.md).
