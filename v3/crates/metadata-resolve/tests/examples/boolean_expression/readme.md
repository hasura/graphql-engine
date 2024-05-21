# note

This test will be used to establish that `old-metadata.json` and
`new-metadata.json` result in the same resolved metadata. Right now, all it
proves it that we can parse `new-metadata.json` successfully, and it produces no
resolved metadata.

The next step is adding something like this to `old-metadata.json`, and trying
to make the metadata recreate it.

```json
[
  {
    "kind": "ObjectBooleanExpressionType",
    "version": "v1",
    "definition": {
      "name": "author_bool_exp",
      "objectType": "author",
      "dataConnectorName": "db",
      "dataConnectorObjectType": "author",
      "comparableFields": [
        {
          "fieldName": "author_id",
          "operators": {
            "enableAll": true
          }
        },
        {
          "fieldName": "first_name",
          "operators": {
            "enableAll": true
          }
        },
        {
          "fieldName": "last_name",
          "operators": {
            "enableAll": true
          }
        }
      ],
      "graphql": {
        "typeName": "Author_bool_exp"
      }
    }
  }
]
```
