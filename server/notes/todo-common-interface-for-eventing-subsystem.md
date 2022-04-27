This note is in [Hasura.Metadata.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Metadata/Class.hs#L39).
It is referenced at:
  - line 107 of [Hasura.Metadata.Class](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Metadata/Class.hs#L107)

# Todo: Common interface for eventing sub-system

Postgres tables' event triggers and scheduled event triggers are similar in the
core logic. But currently, their implementation is completely isolated and do not
share a common schema in Postgres. We're having a plan to simplify them via a
common 'event storage and retrieval' interface (maybe via a Postgres extension?).
This will potentially reduce number of interactions made to database and schema foot print.

TODO: Reference to open issue or rfc?

