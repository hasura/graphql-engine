This note is in [Hasura.Server.Init](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Init.hs#L53).
It is referenced at:
  - line 114 of [Hasura.Server.Types](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Types.hs#L114)

# ReadOnly Mode


This mode starts the server in a (database) read-only mode. That is, only
read-only queries are allowed on users' database sources, and write
queries throw a runtime error. The use-case is for failsafe operations.
Metadata APIs are also disabled.

Following is the precise behaviour -
  1. For any GraphQL API (relay/hasura; http/websocket) - disable execution of
  mutations
  2. Metadata API is disabled
  3. /v2/query API - insert, delete, update, run_sql are disabled
  4. /v1/query API - insert, delete, update, run_sql are disabled
  5. No source catalog migrations are run
  6. During build schema cache phase, building event triggers are disabled (as
  they create corresponding database triggers)

