## Building the binaries


```
npm install
npm run package
```

The binaries will be placed in the `bin` directory at root. Copy the binary to your PATH as `cli-ext`.

## API


```
scaffolder sdl to '{ "types": { "enums": [], "scalars": [], "input_objects": [ { "name": "UserInput", "fields": [ { "name": "username", "type": "String", "description": "lalz" }, { "name": "password", "type": "String!", "description": "pass" } ] } ], "objects": [ { "name": "UserInfo", "fields": [ { "name": "accessToken", "type": "String", "description": "lolz" } ] } ] } }'

```

```
scaffolder sdl from '{ "sdl": { "action": "type Mutation { actionName (arg1: SampleInput!): SampleOutput }", "types": "type SampleOutput { accessToken: String! } input SampleInput { username: String! password: String! }" }, "types": { "scalars": [], "enums": [], "input_objects": [], "objects": [] } }'
```

```
scaffolder scaffold '{ "action": { "action_name": "validatedUserInsert", "action_defn": { "kind": "synchronous", "webhook": "http://192.168.0.107:5000/actions", "arguments": [{ "name": "user", "type": "_user_insert_input!", "description": null }], "output_type": "UserInfo" } }, "types": { "enums": [{ "name": "_user_constraint", "values": [{ "value": "user_pkey", "description": null, "is_deprecated": null }], "description": null }, { "name": "_user_update_column", "values": [{ "value": "email", "description": null, "is_deprecated": null }, { "value": "id", "description": null, "is_deprecated": null }, { "value": "username", "description": null, "is_deprecated": null }], "description": null }, { "name": "_article_constraint", "values": [{ "value": "article_pkey", "description": null, "is_deprecated": null }], "description": null }, { "name": "_article_update_column", "values": [{ "value": "author_id", "description": null, "is_deprecated": null }, { "value": "content", "description": null, "is_deprecated": null }, { "value": "id", "description": null, "is_deprecated": null }, { "value": "title", "description": null, "is_deprecated": null }], "description": null }], "objects": [{ "name": "UserInfo", "fields": [{ "name": "userId", "type": "Int", "arguments": null, "description": null }, { "name": "accessToken", "type": "String", "arguments": null, "description": null }], "description": null, "relationships": [{ "name": "user", "remote_table": "user", "field_mapping": { "userId": "id" } }, { "name": "anotherrel", "remote_table": "article", "field_mapping": { "userId": "id" } }] }, { "name": "SMSInfo", "fields": [{ "name": "sms_id", "type": "String!", "arguments": null, "description": null }], "description": null, "relationships": null }], "scalars": [], "input_objects": [{ "name": "_user_insert_input", "fields": [{ "name": "articles", "type": "_article_arr_rel_insert_input", "description": null }, { "name": "email", "type": "String", "description": null }, { "name": "id", "type": "Int", "description": null }, { "name": "username", "type": "String", "description": null }], "description": null }, { "name": "_article_arr_rel_insert_input", "fields": [{ "name": "data", "type": "[_article_insert_input!]!", "description": null }, { "name": "on_conflict", "type": "_article_on_conflict", "description": null }], "description": null }, { "name": "_article_insert_input", "fields": [{ "name": "author_id", "type": "Int", "description": null }, { "name": "content", "type": "String", "description": null }, { "name": "id", "type": "Int", "description": null }, { "name": "title", "type": "String", "description": null }, { "name": "user", "type": "_user_obj_rel_insert_input", "description": null }], "description": null }, { "name": "_user_obj_rel_insert_input", "fields": [{ "name": "data", "type": "_user_insert_input!", "description": null }, { "name": "on_conflict", "type": "_user_on_conflict", "description": null }], "description": null }, { "name": "_user_on_conflict", "fields": [{ "name": "constraint", "type": "_user_constraint!", "description": null }, { "name": "update_columns", "type": "[_user_update_column!]!", "description": null }, { "name": "where", "type": "_user_bool_exp", "description": null }], "description": null }, { "name": "_user_bool_exp", "fields": [{ "name": "_and", "type": "[_user_bool_exp]", "description": null }, { "name": "_not", "type": "_user_bool_exp", "description": null }, { "name": "_or", "type": "[_user_bool_exp]", "description": null }, { "name": "articles", "type": "_article_bool_exp", "description": null }, { "name": "email", "type": "_String_comparison_exp", "description": null }, { "name": "id", "type": "_Int_comparison_exp", "description": null }, { "name": "username", "type": "_String_comparison_exp", "description": null }], "description": null }, { "name": "_article_bool_exp", "fields": [{ "name": "_and", "type": "[_article_bool_exp]", "description": null }, { "name": "_not", "type": "_article_bool_exp", "description": null }, { "name": "_or", "type": "[_article_bool_exp]", "description": null }, { "name": "author_id", "type": "_Int_comparison_exp", "description": null }, { "name": "content", "type": "_String_comparison_exp", "description": null }, { "name": "id", "type": "_Int_comparison_exp", "description": null }, { "name": "title", "type": "_String_comparison_exp", "description": null }, { "name": "user", "type": "_user_bool_exp", "description": null }], "description": null }, { "name": "_Int_comparison_exp", "fields": [{ "name": "_eq", "type": "Int", "description": null }, { "name": "_gt", "type": "Int", "description": null }, { "name": "_gte", "type": "Int", "description": null }, { "name": "_in", "type": "[Int!]", "description": null }, { "name": "_is_null", "type": "Boolean", "description": null }, { "name": "_lt", "type": "Int", "description": null }, { "name": "_lte", "type": "Int", "description": null }, { "name": "_neq", "type": "Int", "description": null }, { "name": "_nin", "type": "[Int!]", "description": null }], "description": null }, { "name": "_String_comparison_exp", "fields": [{ "name": "_eq", "type": "String", "description": null }, { "name": "_gt", "type": "String", "description": null }, { "name": "_gte", "type": "String", "description": null }, { "name": "_ilike", "type": "String", "description": null }, { "name": "_in", "type": "[String!]", "description": null }, { "name": "_is_null", "type": "Boolean", "description": null }, { "name": "_like", "type": "String", "description": null }, { "name": "_lt", "type": "String", "description": null }, { "name": "_lte", "type": "String", "description": null }, { "name": "_neq", "type": "String", "description": null }, { "name": "_nilike", "type": "String", "description": null }, { "name": "_nin", "type": "[String!]", "description": null }, { "name": "_nlike", "type": "String", "description": null }, { "name": "_nsimilar", "type": "String", "description": null }, { "name": "_similar", "type": "String", "description": null }], "description": null }, { "name": "_article_on_conflict", "fields": [{ "name": "constraint", "type": "_article_constraint!", "description": null }, { "name": "update_columns", "type": "[_article_update_column!]!", "description": null }, { "name": "where", "type": "_article_bool_exp", "description": null }], "description": null }, { "name": "SMSInput", "fields": [{ "name": "sms", "type": "String", "description": null }, { "name": "is_international", "type": "Boolean", "description": null }, { "name": "price", "type": "[Price]", "description": null }], "description": null }, { "name": "Price", "fields": [{ "name": "value", "type": "String", "description": null }], "description": null }] }, "framework": "typescript-express" }'
```

## Templaters

The templaters are present in the `src/templaters` directory. You can modify the templaters and build.
Each templater function must return an array of file objects. Something like:

```
[
  {
    "name": "filename1.js",
    "content": "filename1 content"
  },
  {
    "name": "filename2.js",
    "content": "filename2 content"
  }
]
```

These files will be generated in the PWD of execution.
