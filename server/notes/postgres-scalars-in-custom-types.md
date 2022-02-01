This note is in [Hasura.RQL.DDL.CustomTypes](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/CustomTypes.hs#L27).
It is referenced at:
  - line 101 of [Hasura.RQL.DDL.Action](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Action.hs#L101)
  - line 59 of [Hasura.RQL.DDL.CustomTypes](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/CustomTypes.hs#L59)
  - line 265 of [Hasura.RQL.DDL.CustomTypes](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/CustomTypes.hs#L265)

# Postgres scalars in custom types

It’s very convenient to be able to reference Postgres scalars in custom type
definitions. For example, we might have a type like this:

    type User {
      id: uuid!
      name: String!
      location: geography
    }

The uuid and geography types are Postgres scalars, not separately-defined
GraphQL types. To support this, we have to take a few extra steps:

  1. The set of Postgres base types is not fixed; extensions like PostGIS add
     new ones, and users can even define their own. Therefore, we fetch the
     currently defined base types from the @pg_catalog.pg_type@ system table as part of
     loading the metadata.

  2. It’s possible for a custom type definition to use a type that doesn’t
     appear elsewhere in the GraphQL schema, so we record which base types were
     referenced while validating the custom type definitions and make sure to
     include them in the generated schema explicitly.

