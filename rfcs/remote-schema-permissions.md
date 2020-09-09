# Remote schema permissions

Currently, all remote schemas are fully exposed to all roles. We want to enable role-based access control to remote schemas as well.

There are 2 things we want to focus on (there are possibly more):

1. Role-based schema masking
2. Role-based argument presets

## Role-based schema masking

A PoC of this feature was implemented in https://github.com/hasura/graphql-engine/pull/2690

Schema masking is the ability to hide parts of the schema from the user. By default, we want to enforce "deny all" semantics by not exposing any type for a new role. 
This role can then be allowed access to parts of the schema by specifying the types and fields it should have access to.

The API for role-based schema proposed in #2690 looked like the following (**NOTE:** This is for illustration, not the final API as it doesn't incorporate argument presets):

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
allowed_custom_scalars:
- name: UUID
- name: Geography
```

Only `UUID` and `Geography` custom scalars are allowed in the schema. Note that built-in scalars like `Int`, `String`, etc cannot be masked. 

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
  input_fields: [x]
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
allowed_custom_scalars: "*"
```

In short, any list reference can be replaced by `*` to mean "all valid values" for that list. For example,

1. For an object type, like "User", `fields` can be `*`
2. Even the root level definition can take `*` like `allowed_custom_scalars: *`


## Role-based argument presets

The above API only illustrates schema masking. We need to incorporate argument presets as well to provide "filtered" results. The API described in this section is the final proposed API.

Each field in GraphQL can take arguments as well. Arguments must belong to input types i.e Scalar, Enum, or Input Object (or list of these).

Role-based argument presets can basically inject values from session variables or static values during execution. The presetted argument should also get removed from the exposed role-based schema.

The proposed API:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - name: Query
      fields: 
      - name: hello
      - name: user
        argument_presets:
        - name: "id"
          value:
            from_session_variable: "x-hasura-user-id"
    - name: User
      fields:
      - name: a
      - name: b
      - name: c
```

Argument presets can also go arbitrarily deep if the input type is Input Object (this is very similar to how we define the mapping in remote relationships):

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - name: Query
      fields:
        - name: hello
        - name: user
          argument_presets:
          - name: "where"
            input_field:
            - name: "id"
              input_field:
              - name: "_eq"
                value:
                  from_session_variable: "x-hasura-user-id"
          - name: "limit"
            value:
              static: 1
    - name: User
      fields:
      - name: a
      - name: b
      - name: c
```

There can be more than one argument preset per field:

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    allowed_objects:
    - name: Query
      fields: 
      - name: hello
      - name: user
        argument_presets:
        - name: "id"
          value:
            from_session_variable: "x-hasura-user-id"
        - name: "limit"
          value:
            static: 1
    - name: User
      fields:
      - name: a
      - name: b
      - name: c
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

## Few questions to tackle:

1. This will be a breaking change as all remote schemas will be hidden for all role except `admin`.
This was tackled in #2690 by introducing a feature flag for enabling remote schema permissions explicitly.
2. UX of this feature in the console.
3. The API proposed above is complete in defining the permission for a role and a remote schema. Should we have smaller additive permission API e.g. 
one which takes permissions for a single type (of any type) and adds them together to yield the final schema?
4. How will it work with remote joins?
5. How will it work with multiple roles?
6. Implementation: how do we generate input types for each field when there are presets (currently input types are shared across fields)?
Possible solution: Suffix type name, field name and role name to the role-based type
