# Remote schema permissions

Currently, all remote schemas are fully exposed to all roles. We want to enable role-based access control to remote schemas as well.

There are 2 things we want to focus on (there are possibly more):

1. Role-based schema masking
2. Role-based input field presets

## Role-based schema masking

A PoC of this feature was implemented in https://github.com/hasura/graphql-engine/pull/2690

Schema masking is the ability to hide parts of the schema from the user. By default, we want to enforce "deny all" semantics by not exposing any type for a new role. 
This role can then be allowed access to parts of the schema by specifying the types and fields it should have access to.

The API for role-based schema proposed in #2690 looked like the following (**NOTE:** This is for illustration, not the final API):

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - name: Query
      fields: [hello, user]
    - name: User
      fields: [a,b,c]
```

You may notice that we will need to start with the root types `Query`, `Mutation`, `Subscription` (names might be different for other schemas) to give meaningful access to the role. In the above example, we are essentially saying that: only query fields `hello` (assume scalar type) and `user` (type `User`) are exposted to a role called "user". Further, the type `User` is also restricted to expose the fields `a, b, c` only.

Another way of looking at role-based schema masking is essentially that we are creating role-based types. 

### Behaviour for each type

Schema masking can be done for any GraphQL type. We will describe the effect of schema masking on each type below:

**Object**

```
allowed_objects:
- name: Query
  fields: [hello, user]
- name: User
  fields: [a,b,c]
```

Object types can specify the subset of fields in the type that are allowed in the schema.

**Scalars**

```
allowed_scalars:
- name: String
- name: Int
```

Only `String` and `Int` scalars are allowed in the schema.

**Interfaces**

```
allowed_interfaces:
- name: Shape
  fields: [height, width]
```

Interface type can also specify a subset of fields (similar to object types)

**Unions**

```
allowed_unions:
- name: SearchResult
  objects: [Person]
```

Union types can specify the subset of list of object types.


**Enums**

```
allowed_enums:
- name: Direction
  values: [North, South]
```

Enum types can specify a subset of values for the enum.

**Input Objects**

```
allowed_input_objects:
- name: Point2D
  fields: [x]
```

Input objects can specify a subset of input fields.

#### Wildcard syntax

Since specifying each and every type might be tedious (especially for types that may not really require masking like scalars), we can give provide a wildcard syntax:

```
allowed_objects:
- name: Query
  fields: [hello, user]
- name: User
  fields: "*"
allowed_scalars: "*"
```

In short, any list reference can be replaced by `*` to mean "all valid values" for that list. For example,

1. For an object type, like "User", `fields` can be `*`
2. Even the root level definition can take `*` like `allowed_scalars: *`

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

1. This will be a breaking change as all remote schemas will be hidden for all role except `admin`. This was tackled in #2690 by introducing a feature flag for enabling remote schema permissions explicitly.
