Before we supported volatile functions, the permissions for functions
are automatically inferred. We allowed functions when these conditions are met:

- The function has to be stable/immutable
- The function should return a table type

A stable/immutable function `f` returning a table type `t` is exposed for a
role `r` if there is a select permission defined on table `t` for role `r`.
The rationale behind this decision is that a stable/immutable function does not
modify the database and the data returned by the function is filtered using the
permissions that are specified precisely for that data.

Now consider mutable/volatile functions, we can't automatically infer whether
or not these functions should be exposed for the sole reason that they *can*
modify the database. This necessitates a permission system for functions.

## Permissions on functions

Going forward, a function `f`, stable or volatile is only exposed for a role
`r` if there is a permission defined on the function `f` for the role `r`. A
permission should only be allowed if there is a select permission on the table type.

### Function permission

```yaml
type: create_function_permission
args:
  function: <function_name>
  role: <role_name>
  definition: {}
```

Inside the metadata, it be as follows:

```yaml
version: 3
sources:
  - name: default
    functions:
    - function: some_function
      configuration: {}
      permissions:
      - role: some_role
        definition: {}
```

`definition` is empty for now but we'll have more options as we extend
the feature set.

## Backwards compatibility

Currently stable/immutable functions are exposed automatically, so we need to
preserve this behaviour. So, we can introduce a new flag
`--infer-function-permissions`, the presence of which is an explicit indication
from the user that they want stable/immutable functions which return a table
type to be exposed automatically like before. Note that if the user specifies
permission for a stable function, that is preferred over auto inferred
permission (currently since the permission definition doesn't have anything,
this amounts to the same thing).

## Future work

1. Relax the restrictions on the functions that can be tracked. We can allow
   tracking functions that return a scalar in addition to functions that return
   a table type.

1. Argument presets, can be configured per role for each function. The `definition`
   will look like this:

   ```yaml
    definition:
      argument_presets:
        arg1: ...
        arg2: ...
    ```

1. Ability to override the `filter` of the select permission defined on
   functions returning table types. <TODO: motivate the use case>
