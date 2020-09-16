# Remote schema permissions

Currently, all remote schemas are fully exposed to all roles. We want to enable role-based access control to remote schemas as well.

There are 2 things we want to focus on (there are possibly more):

1. Schema masking for root types: `query_root`, `mutation_root`, `subscription_root`
2. Argument presets

## Schema masking for root types

A PoC of this feature was implemented in https://github.com/hasura/graphql-engine/pull/2690

Schema masking is the ability to hide parts of the schema from the user. The main parts of the schema that a user typically wants to hide are the top-level fields in root types i.e `query_root`, `mutation_root` and `subscription_root` (names may differ depending on schema definition).

The API can look like the following (**NOTE:** This is for illustration, not the final API as it doesn't incorporate argument presets yet):

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    query_permission:
      allowed_fields: [hello, user]
    mutation_permission:
      allowed_fields: [addUser, updateUser]
```

We will not touch other types in v1. By spec, all the types that are reachable from the allowed fields should become part of the schema.

## Argument presets

The above API only illustrates schema masking of "operable" fields. We need to incorporate argument presets as well to provide "filtered" results. The API described in this section is the final proposed API.

Each field in GraphQL can take arguments as well. Arguments must belong to input types i.e Scalar, Enum, or Input Object (or list of these).

Role-based argument presets can basically inject values from session variables or static values during execution. The presetted argument should also get removed from the exposed role-based schema.

The proposed API:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    query_permission:
      allowed_fields:
      - name: hello
      - name: user
        argument_presets:
        - name: "id"
          value:
            from_session_variable: "x-hasura-user-id"
```

Argument presets can also go arbitrarily deep if the input type is Input Object but it must stop at a leaf type (this is very similar to how we define the mapping in remote relationships):

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    query_permission:
      allowed_fields:
        - name: hello
        - name: user
          argument_presets:
          - name: "where"
            - input_field: "id"
              - input_field: "_eq"
                value:
                  from_session_variable: "x-hasura-user-id"
```

There can be more than one argument preset per field:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    query_permission:
      allowed_fields:
      - name: hello
      - name: user
        argument_presets:
        - name: "id"
          value:
            from_session_variable: "x-hasura-user-id"
        - name: "limit"
          value:
            static: 1
```

In the above example, if the client (as role "user") makes the query:

```
query {
  user {
    a
    b
  }
}
```

then the query that is constructed for the remote schema is

```
query {
  user(id: "<x-hasura-user-id>", limit: 1) {
    a
    b
  }
}
```

## Few points to be noted:

1. This will be a breaking change as we want to give default "deny" all fields for remote schemas. We need to introduce a flag (as was done in #2690) to enable remote schema permissions explicitly.
2. For argument presets which are Input Object types, we *could* strip only the input field at the preset path. But we are taking the easier approach of removing the whole argument as it does not involve generating new role-based types.
3. We are not doing response validation. If the upstream graphql service is not graphql compliant, we do not throw any errors.

## Future work

A more generic approach would be for users to define the whole schema for a particular role. They will then need to define a mapping of how the role-based schema maps to the upstream schema. There are some ideas around this (which were discussed in the comments) but it seems best fit for version 2 because i) we haven't heard use-cases which require redefining the whole schema, ii) we will have to do response validation and iii) baby steps :)

If we do get to doing this, we will need to enable this new permission model using a server flag.
