This note is in [Hasura.RQL.DDL.Action](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Action.hs#L89).

# Postgres scalars in action input arguments

It's very comfortable to be able to reference Postgres scalars in actions
input arguments. For example, see the following action mutation:

    extend type mutation_root {
      create_user (
        name: String!
        created_at: timestamptz
      ): User
    }

The timestamptz is a Postgres scalar. We need to validate the presence of
timestamptz type in the Postgres database. So, the 'resolveAction' function
takes all Postgres scalar types as one of the inputs and returns the set of
referred scalars.

