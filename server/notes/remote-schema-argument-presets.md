This note is in [Hasura.RQL.DDL.RemoteSchema.Permission](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/RemoteSchema/Permission.hs#L188).
It is referenced at:
  - line 407 of [Hasura.RQL.Types.RemoteSchema](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/Types/RemoteSchema.hs#L407)

# Remote Schema Argument Presets


Remote schema argument presets are a way to inject values from static values or
from session variables during execution. Presets can be set using the `preset`
directive, the preset directive is defined in the following manner:

```
scalar PresetValue

directive @preset(
  value: PresetValue
) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION
```

When a preset directive is defined on an input type, the input type is removed
from the schema and the value is injected by the graphql-engine during the
execution.

There are two types of preset:

1. Static preset
----------------

Static preset is used to preset a static GraphQL value which will be injected
during the execution of the query. Static presets can be specified on all types
of input types i.e scalars, enums and input objects and lists of these types.
The preset value (if specified) will be validated against the type it's provided.
For example:

```
users(user_id: Int @preset(value: {user_id: 1}))
```

The above example will throw an error because the preset is attempting to preset
an input object value for a scalar (Int) type.

2. Session variable preset
--------------------------

Session variable preset is used to inject value from a session variable into the
graphql query during the execution. If the `value` argument of the preset directive
is in the format of the session variable i.e. `x-hasura-*` it will be treated as a
session variable preset. During the execution of a query, which has a session variable
preset set, the session variable's will be looked up and the value will be constructed
into a GraphQL variable. Check out `resolveRemoteVariable` for more details about how
the session variable presets get resolved.

At the time of writing this note, session variable presets can **only** be specified at
named types and only for scalar and enum types. This is done because currently there's
no good way to specify array or object values through session variables.

