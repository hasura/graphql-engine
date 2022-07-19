This note is in [Hasura.GraphQL.Schema.Node](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Node.hs#L168).
It is referenced at:
  - line 50 of [Hasura.GraphQL.Schema.Relay](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Schema/Relay.hs#L50)

# Internal Relay HashMap


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
*every table parser*, to deal with all possible cases. In practice, we use the
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

