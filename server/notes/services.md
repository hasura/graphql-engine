This note is in [Hasura.Services](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Services.hs#L5).
It is referenced at:
  - line 3 of [Hasura.Services.Network](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Services/Network.hs#L3)

# Services

Different editions of the GraphQL Engine use the same common core, but provide
different features. To avoid having logic deep within the common core that
decides whether a feature is active or not, we favour an "injection" approach:
the core of the engine delegates the implementation details of features /
external dependencies to class constraints, and it's the role of the top-level
caller to implement those constraints.

Those services are implemented on the base monad on which we run the engine. See
'PGMetadataStorageT' in Hasura/App.


