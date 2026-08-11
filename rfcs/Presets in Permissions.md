# Presets in Permissions

As a part of command permissions, we want to be able to control what values for an argument a particular role can pass.

The following changes are proposed to the metadata.

For command permissions, allow specifying a set of argument values which will be preset. Arguments which have been preset no longer get generated in the GraphQL schema.

```yaml
kind: CommandPermissions
version: v1
definition:
  commandName: delete_user
  permissions:
    - role: user
      allowExecution: true
      argumentPresets:
        - argument: user_id
          value:
            sessionVariable: x-hasura-user-id
```

A similar change would be required for model arguments / ModelPermissions.

Also, for supporting complex input objects, we will allow input presets in TypePermissions as well

```yaml
kind: TypePermissions
version: v1
definition:
  typeName: DeleteUserInput
  permissions:
    - role: user
      input:
        fieldPresets:
          - field: is_frozen
            value:
              literal: false
```
Any field which has been preset will not be generated in the GraphQL schema. If all fields of a type have been preset then the parent field / argument will not be generated.
