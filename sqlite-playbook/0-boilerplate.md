# Step 0: Boilerplate

A brief overview of what makes up a backend:

* `data BackendType` needs a case for the backend in
  question, and this needs to be added to `supportedBackends`.

  [Hasura.SQL.Backend](../server/src-lib/Hasura/SQL/Backend.hs)

* `instance Backend`: Defines the associated types that are used for 
  the backend's query AST.

  [Hasura.Backends.SQLite.Instances.Types](../server/src-lib/Hasura/Backends/SQLite/Instances/Types.hs)

* `instance BackendExecute`: Builds the actions for queries and mutations etc.

  [Hasura.Backends.SQLite.Instances.Execute](../server/src-lib/Hasura/Backends/SQLite/Instances/Execute.hs)

* `instance BackendTransport`: Runs the actions built in `BackendExecute`.

  [Hasura.Backends.SQLite.Instances.Transport](../server/src-lib/Hasura/Backends/SQLite/Instances/Transport.hs)

* `instance BackendSchema`: Defines the shape of the graphql queries that the
  backend supports (e.g. what aggregations exist? what can go in a
  `_where`-clause?)

  [Hasura.Backends.SQLite.Instances.Schema](../server/src-lib/Hasura/Backends/SQLite/Instances/Schema.hs)

* `instance BackendMetadata` defines what metadata to track and how.

  [Hasura.Backends.SQLite.Instances.Metadata](../server/src-lib/Hasura/Backends/SQLite/Instances/Metadata.hs)

Once we have the instances defined they also need to be brought into scope in a
few places (in `module Hasura.GraphQL.{Execute,Schema,...}.Instances`)

To get things going, we've cheated a bit and created the necessary boilerplate
beforehand, with holes left for us to fill during this session.

Next up is [Step 1: Add source](1-add-source.md).
