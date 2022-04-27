This note is in [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L252).
It is referenced at:
  - line 759 of [Hasura.GraphQL.Transport.WebSocket](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Transport/WebSocket.hs#L759)
  - line 245 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L245)
  - line 444 of [Hasura.Server.Logging](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/Logging.hs#L444)

# Disable query printing when query-log is disabled

As a temporary hack (as per https://github.com/hasura/graphql-engine-mono/issues/1770),
we want to print the graphql query string in `http-log` or `websocket-log` only
when `query-log` is enabled.

