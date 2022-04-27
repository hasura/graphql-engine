This note is in [Hasura.GraphQL.Execute.RemoteJoin.Collect](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/RemoteJoin/Collect.hs#L30).

# Remote Joins Architecture


    Unparsed Incoming GraphQL   +------------------------------+
    --------------------------> | Parsing of the GraphQL query |-----+
                                +------------------------------+     |
                                                                     | DB Query and remote joins (if any)
                                                                     |
                                                                     V
    +----------------------------------+  SQL query response  +----------------------------+
    |  Traverse the DB response to     | <------------------- |  Execution of the DB query |
    |  get the values of the arguments |                      +----------------------------+
    |   of the remote field            |
    +----------------------------------+
                 |
                 | Remote field arguments
                 V
    +--------------------------+  Remote schema response   +----------------------------------------+
    | Query the remote schema  | ------------------------> | Replace the remote join fields in      |
    | with the remote field    |                           | the SQL query response (JSON) with     |
    | arguments to the remote  |                           | the response obtained from the remote  |
    | field configured in the  |                           | schema at appropriate places.          |
    | remote join.             |                           +----------------------------------------+
    +--------------------------+

