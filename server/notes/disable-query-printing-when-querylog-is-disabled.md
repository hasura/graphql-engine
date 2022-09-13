This note is in [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L251).
It is referenced at:
  - line 793 of [Hasura.GraphQL.Transport.WebSocket](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Transport/WebSocket.hs#L793)
  - line 244 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L244)
  - line 445 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L445)

# Disable query printing when query-log is disabled

As a temporary hack (as per https://github.com/hasura/graphql-engine-mono/issues/1770),
we want to print the graphql query string in `http-log` or `websocket-log` only
when `query-log` is enabled.

