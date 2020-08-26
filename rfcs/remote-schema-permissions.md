# Remote schema permissions

Currently, all remote schemas are fully exposed to all roles. We want to enable role-based access control to remote schemas as well.

There are 2 things we want to focus on (there are possibly more):

1. Role-based schema masking
2. Role-based input field presets

## Role-based schema masking

A PoC of this feature was implemented in https://github.com/hasura/graphql-engine/pull/2690

Schema masking is the ability to hide parts of the schema from the user. By default, we want to enforce "deny all" semantics by not exposing anything for a new role. 
This role can then be allowed access to parts of the schema by specifying the type and the list of fields it should have access to.

The API for role-based schema proposed in #2690 looked like the following (**NOTE:** This is for illustration, not the final API):

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - type: Query
      fields: [hello, user]
    - type: User
      fields: [a,b,c]
```

You may notice that we will need to start with the root types `Query`, `Mutation`, `Subscription` (names might be different for other schemas) to give meaningful access to the role. In the above example, we are essentially saying that: only query fields `hello` (assume scalar type) and `user` (type `User`) are exposted to a role called "user". Further, the type `User` is also restricted to expose the fields `a, b, c` only.

Another way of looking at role-based schema masking is essentially that we are creating role-based types. 

We will modify the above API slightly to incorporate input field presets below. This is the actual proposed API for remote schema permissions.


## Role-based input field presets

Each field in GraphQL can take arguments as well (aka input fields). Input fields can only be of type (including List of these): Scalar, Enum, or Input Object. This set is also called Input Types.

Role-based input field presets can basically inject values from session variables or static values during execution. The presetted input field should also get removed from the exposed role-based schema.

The proposed API:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - type: Query
      fields: 
        - name: hello
        - name: user
          input_field_presets:
            - name: "id"
              value: 
                from_session_variable: "x-hasura-user-id"
    - type: User
      fields: [a,b,c]
```

Input field presets can also go arbitrarily deep if the input field type is Input Object (this is very similar to how we define the mapping in remote relationships) and there can be multiple presets per field:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - type: Query
      fields: 
        - name: hello
        - name: user
          input_field_presets:
            - name: "where"
              field:
                - name: "id"
                  field:
                    - name: "_eq"
                      value: 
                        from_session_variable: "x-hasura-user-id"
            - name: "limit"
              value:
                static: 1
    - type: User
      fields: [a,b,c]
```

In the above example, if the client (as role "user") makes the query:

```
query {
  user {
    name
    email
  }
}
```

then the query that is constructed for the remote schema is

```
query {
  user(where: { id: { _eq: "x-hasura-user-id" } }, limit: 1) {
    name
    email
  }
}
```

## Few questions to tackle:

1. The above example only shows "allowed_objects". We should only consider how to tackle the other GraphQL types: Scalars, Interfaces, Unions, Enums and Input Objects
2. This will be a breaking change as all remote schemas will be hidden for all role except `admin`. This was tackled in #2690 by introducing a feature flag for enabling remote schema permissions explicitly.
