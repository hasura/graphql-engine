This note is in [Hasura.RQL.DDL.RemoteSchema.Permission](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/RemoteSchema/Permission.hs#L234).

# Remote Schema Permissions Architecture


The Remote schema permissions feature is designed in the following way:

1. An user can configure remote schema permissions for a particular role using
   the `add_remote_schema_permissions` API, note that this API will only work
   when remote schema permissions are enabled while starting the graphql-engine,
   which can be done either by the setting the server flag
   `--enable-remote-schema-permissions` or the env variable
   `HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS` to `true`. Check the module
   documentation of `Hasura.RQL.DDL.RemoteSchema.Permission` (this module) for
   more details about how the `add_remote_schema_permissions` API works.
2. The given schema document is parsed into an `IntrospectionResult` object,
3. The schema is built with the `IntrospectionResult` parsed in #2 for the said role.
   Check out the documentation in `argumentsParser` to know more about how the presets
   are handled.
4. For a remote schema query, the schema will return a `RemoteField` which
   contains unresolved session variables, the `RemoteField` is resolved using the
   `resolveRemoteField` function. The `resolveRemoteVariable` function contains more
   details about how the `RemoteVariable` is resolved.
5. After resolving the remote field, the remote server is queried with the resolved
   remote field.

