## Rename type and field names for Remote Schemas

When adding a remote schema (or after adding), users can be given a choice to rename
types and fields so that they are graphql-conformant, avoid conflicts with already existing schemas, or simply for better customization.

There are 4 things that we can do for Remote Schemas:

1. Namespacing
2. Customize type names
3. Add global type prefix
4. Customize root field names

#### Namespacing

`add_remote_schema` can take a `namespace` key. If this key is set, then the fields of the remote schema are added under the namespace respectively i.e. an object field with name `namespace`  is added inside `query_root`, `mutation_root` and `subscription_root` and the remote schema fields are added inside this object field.

E.g. suppose user provides namespace “userservice” when adding remote schema.

```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
            "url": "https://remote-server.com/graphql",
            "forward_client_headers": false
        },
        "customization": {
          "namespace": "userservice"
        }
    }
}
```

Then you can query as:

```
query {
  userservice {
    getUser
}

mutation {
  userservice {
    deleteUser
}
```

#### Customize type names

When adding a remote schema, a custom spec for renaming types can be given.

```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
          "url": "https://remote-server.com/graphql",
          "forward_client_headers": false
        },
        "customization": {
           "custom_types": [
             {
               "name": "User",
               "new_name": "RemoteUser"
             }
           ]
        }
    }
}
```


#### Add global type prefix

Although the above solves the problem of customizing individual types, we can also provide an
option to add a global prefix to all types. This allows in automatic customization in case 
the downstream graphql schema are changed.

```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
            "url": "https://remote-server.com/graphql",
            "forward_client_headers": false
        },
        "customization": {
          "custom_type_prefix": “userservice”
        }
    }
}
```

Note that if both `custom_type_prefix` and `custom_types` are given, then `custom_types` is given priority i.e. `custom_type_prefix` is applied to all types other than those given in `custom_types`.


#### Customize root field names

Similar to customizing type names, we can also customize root field names.


```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
          "url": "https://remote-server.com/graphql",
          "forward_client_headers": false
        },
        "customization": {
           "custom_root_fields": [
             {
               "name": "getUser",
               "new_name": "userserviceGetUser"
             }
           ]
        }
    }
}
```

Then you can run the query as:

```
query {
  userserviceGetUser { .. }
}

```
