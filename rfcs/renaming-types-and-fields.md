## Rename type and field names for Remote Schemas

When adding a remote schema (or after adding), users can be given a choice to rename
types and fields so that they are graphql-conformant, avoid conflicts, or simply more usable.

There are 2 independent things that are required for Remote Schemas:

1. Namespacing
2. Customise type and field names (also enable prefixing)

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
            "forward_client_headers": false,
            "namespace": “userservice”
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

#### Customise type and field names 

When adding a remote schema, a custom spec for renaming types and fields can be given.

```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
          "url": "https://remote-server.com/graphql",
          "forward_client_headers": false,
          "custom_types": [
            {
              "type": "Query",
              "custom_fields": [
                {
                  "field": "getUser",
                  "new_field": "getRemoteUser"
                },
                {
                  "field": "getUser2",
                  "new_field": "getRemoteUser2"
                }
              ]
            },
            {
              "type": "User",
              "new_type": "RemoteUser"
            }
          ]
      }
    }
}
```

The main parameters here are `new_type` and `new_field`.

##### Prefixing

Although the above solves the problem of customizing individual types and fields, we can also provide an
option to add a global prefix to all types and field names. This allows in automatic customization in case 
the downstream graphql schema are changed.

```
{
    "type": "add_remote_schema",
    "args": {
        "name": "my remote schema",
        "definition": {
            "url": "https://remote-server.com/graphql",
            "forward_client_headers": false,
            "custom_prefix": “userservice”
        }
    }
}
```


```
query {
  userservice_getUser
}

mutation {
  userservice_deleteUser
}
```
