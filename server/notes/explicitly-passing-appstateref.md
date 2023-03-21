This note is in [Hasura.Server.App](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/App.hs#L437).
It is referenced at:
  - line 481 of [Hasura.Server.App](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/App.hs#L481)

# Explicitly passing AppStateRef

The AppStateRef is passed explicitly to `v1QueryHandler` and `v1MetadataHandler`
functions, so that they can update the schema cache in the ref.
They don't use it to read the latest AppContext or SchemaCache.
The AppContext or SchemaCache is read from the HandlerCtx.
This is to avoid any race conditions that can occur by reading AppContext/SchemaCache
one after the other.

