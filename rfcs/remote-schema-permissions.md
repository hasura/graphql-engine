# Remote schema permissions

Currently, all remote schemas are fully exposed to all roles. We want to enable role-based access control to remote schemas as well.

There are 2 aspects to this:

1. Role-based schema (which is a "subset" of original schema)
2. Argument presets for fields

## Role-based schemas

A user should have the ability to define what parts of the schema they want to expose to the user. This helps in removing fields and changing types that they perhaps don't want to expose to a particular role. The fields in the schema should be a subset of the upstream schema, otherwise upstream will not know how to execute them.

```
type: add_remote_schema_permissions
args:
  remote_schema: comm
  role: user
  definition:
    schema: |

      schema {
        query: query_root
        mutation: mutation_root
      }

      type User {
        id Int!
        name String!
      }

      input UserBoolExp {
        id: IntBoolExp
        name: StringBoolExp
      }

      input IntBoolExp {
        _eq: Int!
        _lte: Int!
        _gte: Int!
      }

      input StringBoolExp {
        _eq: String!
        _contains: String!
      }

      type query_root {
        hello: String
        user(id: Int): User
        users(where: UserBoolExp): [User]
      }
```

Apart from validating the schema for spec compliance, we will need to validate the schema against the upstream schema (to make sure there are no dangling fields).

When an operation is executed against this schema, we will also need to type check the response to be valid for the schema.

## Argument presets

The role-based schema only helps in changing the type definitions that are exposed. We also want to incorporate argument presets for fields to embed constraints in operations.

Each field in GraphQL can take arguments that belong to input types i.e Scalar, Enum, or Input Object (and lists of these).

Argument presets will basically inject values from session variables or static values during execution. The presetted argument should also get removed from the exposed role-based schema. We will define argument presets using a `@preset` directive in the role-based schema.

For brevity, we will hide the non-relevant parts of the schema:

```
scalar PresetValue # This custom scalar can be added by Hasura implicitly

directive @preset(
  value: PresetValue
) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION   # The directive definition can be added by Hasura implicitly

type query_root {
  hello: String
  user(id: Int @preset(value: "x-hasura-user-id")): User   # argument "id" gets removed from the exposed schema
  users(where: UserBoolExp @preset(value: { id : {_eq: "x-hasura-user-id"}}) ): [User]   # argument "where" gets removed from the exposed schema
}
```

For input objects presets, we can allow user to customize the argument type by providing new input object types (the new input object should also be a subset of the original input object). This way some input fields can be preset and remaining can be exposed in the schema :

```
input UserBoolExpForRole { # this is a new type defined by the user for just this role schema, it is a duplicate of 'UserBoolExp' but with a preset
   id: IntBoolExp @preset(value = { _eq: "x-hasura-user-id"}) # field "id" is removed from the exposed schema
   name: StringBoolExp
}

type query_root {
  hello: String
  user(id: Int @preset(value = "x-hasura-user-id")): User
  users(where: UserBoolExpForRole): [User]  # in this case, argument "where" is not removed from the exposed schema
}
```

Again, we will need to type check the preset values here. One problem to tackle is how do we differentiate static values from session variables (suppose the static value is, in the off-chance, "x-hasura-user-id"). The solution here can be the introduction of another directive parameter `@static` (or `@literal`) which can be provided to parse a session variable to it's literal value:

```
@preset(value: { id : {_eq: "x-hasura-user-id"}, something_else: 10}, static: true)
```

## Points to be noted:

1. This will be a breaking change as we want to give default "deny" all fields for remote schemas. We need to introduce a flag (as was done in #2690) to enable remote schema permissions explicitly.
2. The first version can skip allowing new types in the role-based schema (e.g. second example in Argument Presets section).
3. For remote schema relationships, the relationship field should be available inside the table selection set only if the corresponding remote schema field is available in the role-based schema. All preset arguments that are part of the join condition are ignored for the remote schema field as the argument values will be coming from the relationship definition. In other words, join condition takes precedence over argument presets.

## Future work

1. Since we are doing response validation, we will be able to add additional directives which can operate on the response data (in future). For example, i) a `nullif(bool_cond)` directive on fields which replaces the value of the field with `null` if the `bool_cond` evalues to `true`. ii) a `filter(bool_cond)` on object types, which only sends values for which `bool_cond` evalues to `true` for that object type in the response.
