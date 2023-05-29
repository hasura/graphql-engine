-- | A collection of types and utilities around the @Node@ GraphQL
-- type exposed by the Relay API.
module Hasura.GraphQL.Schema.Node
  ( -- * Node id
    NodeId (..),
    V1NodeId (..),
    V2NodeId (..),

    -- * Node id version
    NodeIdVersion,
    nodeIdVersionInt,
    currentNodeIdVersion,

    -- * Internal relay types
    NodeMap,
    TableMap (..),
    NodeInfo (..),
    findNode,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache

{- Note [Relay Node Id]
~~~~~~~~~~~~~~~~~~~~~~~

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
-}

--------------------------------------------------------------------------------
-- Node id

data NodeId
  = NodeIdV1 V1NodeId
  | NodeIdV2 (AB.AnyBackend V2NodeId)

-- | V1 format of a node.
--
-- This id does NOT uniquely identify the table properly, as it only knows the
-- table's name, but doesn't store a source name.
data V1NodeId = V1NodeId
  { _ni1Table :: Postgres.QualifiedTable,
    _ni1Columns :: NESeq.NESeq J.Value
  }

-- | V2 format of a node.
--
-- Uniquely identifies a table with source name and table name, and uniquely
-- identifies a row within that table with a list of primary key values.
data V2NodeId b = V2NodeId
  { _ni2Source :: SourceName,
    _ni2Table :: TableName b,
    _ni2Columns :: NESeq.NESeq J.Value
  }

instance J.FromJSON NodeId where
  parseJSON = J.withArray "node id" \array -> case toList array of
    [] -> fail "unexpected GUID format, found empty list"
    J.Number 1 : rest -> NodeIdV1 <$> parseNodeIdV1 rest
    J.Number n : _ -> fail $ "unsupported GUID version: " <> show n
    _ -> fail "unexpected GUID format, needs to start with a version number"

parseNodeIdV1 :: [J.Value] -> J.Parser V1NodeId
parseNodeIdV1 (schemaValue : nameValue : firstColumn : remainingColumns) =
  V1NodeId
    <$> (Postgres.QualifiedObject <$> J.parseJSON schemaValue <*> J.parseJSON nameValue)
    <*> pure (firstColumn NESeq.:<|| Seq.fromList remainingColumns)
parseNodeIdV1 _ = fail "GUID version 1: expecting schema name, table name and at least one column value"

--------------------------------------------------------------------------------
-- Node id version

-- | Enum representing the supported versions of the API.
data NodeIdVersion
  = NIVersion1
  | NIVersion2
  deriving (Show, Eq)

nodeIdVersionInt :: NodeIdVersion -> Int
nodeIdVersionInt = \case
  NIVersion1 -> 1
  NIVersion2 -> 2

currentNodeIdVersion :: NodeIdVersion
currentNodeIdVersion = NIVersion1

--------------------------------------------------------------------------------
-- Internal relay types

{- Note [Internal Relay HashMap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parsing the Node interface
--------------------------

When selecting a node in the schema, the user can use GraphQL fragments to
select different rows based on what table the node id maps to. For instance, a
Relay query could look like this (assuming that there are corresponding tables
"album" and "track" in the schema, possibly in different sources):

    query {
      node(id: "base64idhere") {
        ... on album {
          albumName
        }
        ... on track {
          trackName
        }
      }
    }

What that means is that the parser for the 'Node' interface needs to delegate to
\*every table parser*, to deal with all possible cases. In practice, we use the
'selectionSetInterface' combinator (from Hasura.GraphQL.Parser.Internal.Parser):
we give it a list of all the parsers, and it in turn applies all of them, and
gives us the result for each possible table:
  - if the table was "album", the parsed result is: ...
  - if the table was "track", the parsed result is: ...

The parser for the interface itself cannot know what the actual underlying table
is: that's determined by the node id, which is not something inherent to the
interface! Consequently, what the parser for the interface returns is a
container, that to every supported table in the schema, associates the
corresponding parser output; the node *field* can then use that map and the node
id it got as an argument to extract the relevant information out of said
container.

The 'NodeMap' container
-----------------------

To avoid having to do extra lookups, we also store in that container additional
information about the table: permissions for the current role, connection
information... so that the field, by simply doing a lookup based on the node id,
can have all the information it needs to craft a corresponding query.

In practice: the value we store in our container is a 'NodeInfo' (see
below). Our container, in turn, isn't a 'HashMap' from "unique table identifier"
to 'NodeInfo'; the problem is that not all sources have the same backend type,
meaning that the "unique table identifier" would need to be a _hetereogeneous_
key type. This can be achieved with a dependent map (such as
Data.Dependent.Map.DMap), but is extremely cumbersome. Instead, our overall
container, 'NodeMap', is two layers of 'HashMap': to a source name, we associate
a "backend-erased" 'TableMap' which, in turn, for the corresponding backend,
associates to a table name the corresponding 'NodeInfo'.

Module structure
----------------

Ideally, none of those types should be exported: they are used in the return
type of 'nodeInteface', but consumed immediately by 'nodeField' (see both in
Relay.hs), and they could therefore be purely internal... except for the fact
that 'Common.hs' needs to know about the NodeMap, which is why it is defined
here instead of being an implementation detail of 'Relay.hs'.
-}

type NodeMap = HashMap SourceName (AB.AnyBackend TableMap)

-- | All the information required to craft a query to a row pointed to by a
-- 'NodeId'.
data NodeInfo b = NodeInfo
  { nvSourceInfo :: SourceInfo b,
    nvSelectPermissions :: SelPermInfo b,
    nvPrimaryKeys :: PrimaryKeyColumns b,
    nvAnnotatedFields :: IR.AnnFieldsG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)
  }

newtype TableMap b = TableMap (HashMap (TableName b) (NodeInfo b))

-- | Given a source name and table name, peform the double lookup within a
-- 'NodeMap'.
findNode :: forall b. (Backend b) => SourceName -> TableName b -> NodeMap -> Maybe (NodeInfo b)
findNode sourceName tableName nodeMap = do
  anyTableMap <- HashMap.lookup sourceName nodeMap
  TableMap tableMap <- AB.unpackAnyBackend @b anyTableMap
  HashMap.lookup tableName tableMap
