This note is in [Hasura.RQL.DDL.CustomTypes](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/CustomTypes.hs#L80).
It is referenced at:
  - line 118 of [Hasura.RQL.DDL.Action](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Action.hs#L118)
  - line 325 of [Hasura.RQL.DDL.CustomTypes](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/CustomTypes.hs#L325)

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

We currently have no plan to extend that functionality to other backends; if we
do, we will probably choose to prefix such types with an explicit tag to avoid
having to disambiguate type names across backends.

