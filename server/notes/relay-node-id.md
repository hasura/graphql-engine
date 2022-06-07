This note is in [Hasura.GraphQL.Schema.Node](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Node.hs#L36).
It is referenced at:
  - line 379 of [Hasura.Backends.Postgres.Translate.Select.Internal.Process](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/Postgres/Translate/Select/Internal/Process.hs#L379)

# Relay Node Id


Relay API
---------

The 'Node' interface in the Relay API schema has exactly one field, which
returns a non-null 'ID' value. In a backend that supports the Relay API, each
table's corresponding GraphQL object implements that interface, and provides an
@id@ field that uniuqely identifies each row of the table. See
https://relay.dev/graphql/objectidentification.htm for more details.

To uniquely identify a given row in a given table, we use two different pieces
of information:
  - something that uniquely identifies the table within the schema
  - something that uniquely identifies the row within the table

Both V1 and V2 (of this particular API, not of the engine, see 'NodeIdVersion')
use the same data to uniquely identify the row within the table: a list of
values that map to the table's primary keys, in order. Where they differentiate
is on how they identify the table within the schema:
  - V1 only used a Postgres table name;
  - V2 uses a source name, and a backend-agnostic table name

For now, we still only emit and accept V1 ids: switching to emitting V2 node ids
will be a breaking change that will we do soon. We will continue to accept V1
node ids after that change, meaning we still to resolve them; in practice, that
means iterating over all the Postgres sources, until we find one that has a
table with the given name. If we find more than one, then we fail, to avoid
having to pick a random one (and potentially silently return wrong results.)

Id format
---------

All the required information is encoded into a unique node id using the
following pipeline:

    values <-> JSON array <-> bytestring <-> base64 string

In v1, the content of the JSON array was:

    [ 1         -- JSON number: version number
    , "public"  -- JSON string: Postgres schema name
    , "foo"     -- JSON string: Postgres table name
    , ...       -- arbitrary JSON values: values for each primary key, in order
    ]

As of v2, the content of the JSON array is as follows:

    [ 2                    -- JSON number: version number
    , "default"            -- JSON string: source name
    , "postgres"           -- JSON string: backend type
    , { "schema: "public"  -- arbitrary JSON value: table name in that backend
      , "name": "foo"
      }
    , ...                  -- arbitrary JSON values: values for each primary key, in order
    ]

Encoding and decoding
---------------------

The encoding of a given row's id is performed in each backend's translation
layer, as crafting the row's id requires extracting information out of the
database (the primary key values). Selecting the 'id' field of a compatible
table will yield an 'AFNodeId' field in the IR (see Hasura.RQL.IR.Select), that
each compatible backend will then interpret appropriately.

Decoding, however, does not require introspecting the database, and is performed
at parsing time, so that we can select the corresponing table row. See
'nodeField' in 'Relay.hs' for more information.

