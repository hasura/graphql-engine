This note is in [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L264).
It is referenced at:
  - line 840 of [Hasura.GraphQL.Transport.WebSocket](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Transport/WebSocket.hs#L840)
  - line 257 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L257)
  - line 467 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L467)

# Disable query printing when query-log is disabled

As a temporary hack (as per https://github.com/hasura/graphql-engine-mono/issues/1770),
we want to print the graphql query string in `http-log` or `websocket-log` only
when `query-log` is enabled.

