## Improvements to GraphQL API over SQL functions

Currently we only support stable/immutable functions which return a list of rows.
The rationale behind this decision was to avoid building a permission system
for functions. The permissions are implicitly inferred as follows:

> A function is allowed for a role only if the table it returns is allowed.

This assumption is safe for stable/immutable functions as
1. The functions cannot modify the database.
2. The user will only see the data that is allowed through configured permissions
   on the return tables.

However the above system is insufficient to expose functions which are not
`stable` or `immutable` - when the functions mutate the database, they cannot
be exposed even if there is a select permission on the return type. This
necessitates a permission system that is designed for functions to help us move
past the `stable`/`immutable` requirement.


### Proposal

1. Every function is allowed to be exposed through GraphQL API. If a function
   is marked as `immutable`/`stable`, it is exposed under `query_root`
   otherwise under `mutation_root`.

2. A function `f` is only exposed for a role `r` in the GraphQL API if a
   permission is defined on `f` for the role `r`.

### Permissions

#### Argument presets

Either a static value or something that can be filled from a session variable.

#### Permissions on the return types

1. If the return type is a scalar, no further configuration is allowed.
1. If the return type is a table type or setof <table> type that is already
   tracked, we can maybe allow overriding the filter? Note that the allowed columns
   cannot be changed without changing the name of the type.

### Backwards compatibility

Since we currently automatically expose functions, we need a command line
flag such as `ENABLE_FUNCTION_PERMISSIONS` (which defaults to false) to
enable permissions for maintaining backwards compatibility.
