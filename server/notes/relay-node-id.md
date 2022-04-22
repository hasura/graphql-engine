This note is in [Hasura.GraphQL.Schema.Select](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Select.hs#L1568).
It is referenced at:
  - line 374 of [Hasura.Backends.Postgres.Translate.Select.Internal.Process](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/Translate/Select/Internal/Process.hs#L374)
  - line 1600 of [Hasura.GraphQL.Schema.Select](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Select.hs#L1600)

# Relay Node Id


The 'Node' interface in Relay schema has exactly one field which returns
a non-null 'ID' value. Each table object type in Relay schema should implement
'Node' interface to provide global object identification.
See https://relay.dev/graphql/objectidentification.htm for more details.

To identify each row in a table, we need to encode the table information
(schema and name) and primary key column values in the 'Node' id.

Node id data:
-------------
We are using JSON format for encoding and decoding the node id. The JSON
schema looks like following

'[<version-integer>, "<table-schema>", "<table-name>", "column-1", "column-2", ... "column-n"]'

It is represented in the type @'NodeId'. The 'version-integer' represents the JSON
schema version to enable any backward compatibility if it is broken in upcoming versions.

The stringified JSON is Base64 encoded and sent to client. Also the same
base64 encoded JSON string is accepted for 'node' field resolver's 'id' input.

