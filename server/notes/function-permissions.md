This note is in [Hasura.RQL.DDL.Schema.Function](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Function.hs#L190).
It is referenced at:
  - line 566 of [Hasura.GraphQL.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema.hs#L566)
  - line 569 of [Hasura.GraphQL.Schema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema.hs#L569)

# Function Permissions

Before we started supporting tracking volatile functions, permissions
for a function was inferred from the target table of the function.
The rationale behind this is that a stable/immutable function does not
modify the database and the data returned by the function is filtered using
the permissions that are specified precisely for that data.
Now consider mutable/volatile functions, we can't automatically infer whether or
not these functions should be exposed for the sole reason that they can modify
the database. This necessitates a permission system for functions.
So, we introduce a new API `pg_create_function_permission` which will
explicitly grant permission to a function to a role. For creating a
function permission, the role must have select permissions configured
for the target table.
Since, this is a breaking change, we enable it only when the graphql-engine
is started with
`--infer-function-permissions`/HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS set
to false (by default, it's set to true).

