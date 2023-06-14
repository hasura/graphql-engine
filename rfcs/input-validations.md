# Input Validations

## Index

- [Validation for mutations](#validation-for-mutations)
  * [Mutation performance](#mutation-performance)
- [Insert Mutation](#insert-mutations)
  * [Behaviour](#behaviour)
  * [Webhook specification](#webhook-specification)
    * [Request](#request)
    * [Response](#response)
  * [Examples](#examples)
- [Update Mutation](#update-mutations)
  * [Behaviour](#behaviour-1)
  * [Webhook specification](#webhook-specification-1)
    * [Request](#request-1)
    * [Response](#response-1)
  * [Examples](#examples-1)
- [Delete Mutation](#delete-mutations)
  * [Behaviour](#behaviour-2)
  * [Webhook specification](#webhook-specification-2)
    * [Request](#request-2)
    * [Response](#response-2)
  * [Examples](#examples-2)
- [Enhancements](#enhancements)

Any mutation input coming from an end user should be allowed to be validated.

For eg: Consider the following mutation:
```graphql
mutation insertUser($email:String, $name: String) {
  insert_users(objects: [{email: $email, name: $name}]) {
    affected_rows
    returning {
      id
    }
  }
}
```
We might want to validate that the value provided for the email field is valid. And 
also restrict the number of row inserts that can happen. Currently, this
is not possible in Hasura.

With this RFC we want to give the ability to hook an external (or internal) function
for the purposes of validating the arguments provided for a mutation.

## Validation for mutations

A new field called `validate_input` has been introduced in the insert/update/delete
permission definition to configure the validation.

Find a sample configuration below.

```yaml
type: pg_create_(insert|update|delete)_permission

args:
  table: article,
  source: default,
  role: user,
  permission:
    validate_input:
      type: http
      definition:
        url: http://www.somedomain.com/validateArticle
        headers:
        - name: X-Validate-Input-API-Key
          value_from_env: VALIDATION_HOOK_API_KEY
        forward_client_headers: true
        timeout: 5
```

The `type` determines the interface for the input validation, which initially only
supports `http` webhook url. However, we may expand support for multiple interfaces
such as a Postgres function or a remote schema field.

The `definition` field provides necessary context for communicating and submitting
the data for input validation. It is an object with the following fields.
- `url` - *Required*, a [string value](https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl) which supports templating environment variables.
- `headers` - *Optional*, List of defined headers to be sent to the url.
- `forward_client_headers` - *Optional*, default is `false`. If set to `true` the client headers are forwarded to the url.
- `timeout` - *Optional*, default is `10`. Number of seconds to wait for response before timing out.

### Mutation performance

Mutations that involve input validation may exhibit slower performance compared to mutations without validation.
The execution time of the webhook url can become a bottleneck, potentially reaching the maximum limit specified
by the `timeout` configuration value.

## Insert Mutations

### Behaviour

When an insert mutation comes in with a role, the following steps are performed:

1. First "collect" all the tables that the mutation targets (because there could be
   more than one table involved via nested inserts)
2. If there is a `validate_input` permission on a table, then any arguments targeting
   that table are sent to the `url`. This is done for all tables.
3. If all handlers validates the insert data (_mutation arguments_), then the request
   proceeds. **A transaction with the database will only be started after the
   validation is over and successful.**
4. If any url invalidates the insert data, then the request aborts. An `error`
   message from the url can also be forwarded to the client.


Consider the following sample mutation:
```graphql
mutation insertAuthorWithArticles($name: String, $email:String, $articles_content:[article_insert_input!]!) {
  insert_author(objects: {name: $name, email: $email, articles: {data: $articles_content}}){
    returning {
      first_name
      articles {
        id
      }
    }
  }
}
```
The mentioned mutation targets the `author` and `article` tables, involving a nested
insert of article into the author model. Assuming that the `validate_input`
permission is defined for both tables, the validation process unfolds as follows:

- The validation webhook specified for the `author` table is contacted first,
  including the inserted row with `articles` data.
- Subsequently, the validation webhook designated for the article table is contacted
  with `$articles_content` rows.
- If both of the above webhook calls result in successful validation, a database
  transaction is initiated to insert the rows into the respective tables.

### Webhook specification

### Request

When an insert mutation on a table with `validate_input` configuration is executed,
before making a database transaction Hasura sends the insert data to the validation
HTTP webhook using a `POST` request.

The request payload is of the format:

```json
{
    "version": "<version-integer>",
    "role": "<role-name>",
    "session_variables": {
        "x-hasura-user-id": "<session-user-id>",
        "x-hasura-user-name": "<session-user-name>"
    },
    "data": {
        "input": [JSON-fied <model_name>_insert_input!]
    }
}
```

- `version`: An integer version serves to indicate the request format. Whenever a
breaking update occurs in the request payload, the version will be incremented. The
initial version is set to `1`.
- `role`: Hasura session role on which permissions are enforced.
- `session_variables`: Session variables that aid in enforcing permissions. Variable
  names always starts with `x-hasura-*`.
- `data.objects`: List of rows to be inserted which are specified in the `objects`
input field of insert mutation. Also includes nested data of relationships. The
structure of this field will be similar to the JSONified structure of the
`<model_name>_insert_input!` graphql type.

Note that, in `data.objects` if the data to be inserted contains nested inserts, then the `data.objects` for the:
1. The Root model has a type of `JSON-fied <model_name>_insert_input!`, i.e the nested inserts will be present as 
`JSON-fied <model_name>_(arr|obj)_rel_insert_input!`
2. The nested inserts payload has the type `JSON-fied <nested_model_name>_insert_input!`

### Response

1. Successful Response

The HTTP validation URL should return a `200` status code to represent successful
validation.

```http
200 OK
```

2. Unsucessful Response

The HTTP validation URL should return a optional JSON object with `400` status
code to represent failed validation. The object should contain `message` field
whose value is a string and this message is forwarded to client.

If no JSON object is returned then no message is forwarded to client.


```http
400 BAD REQUEST

{
    "message": "Phone number invalid"
}
```
When an unexpected response format is received, Hasura raises internal exception.

### Examples

1. [Single model] Check if `email` is valid when creating a `user`

Consider the following mutation:

```graphql
mutation insertUser($email:String, $name: String) {
  insert_users(objects: [{email: $email, name: $name}]) {
    affected_rows
    returning {
      id
    }
  }
}
```


The arguments used in `users` model i.e. `$email` and `$name` are sent to the
`validate_input` url of `users` insert permission. The url can check the `email`
value. If the url returns success, then the mutation proceeds else the error from the
url is forwarded.


The sample payload the validation URL of `user` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
        "input": [
            {
                "name": "Jane",
                "email": "jane@b.com",
            },
            {
                "name": "Doe",
                "email": "doe@b.com",
            }
        ]
    }
}
```

2. [Multiple models] Check if `article` length is less than 1000 when inserting an `author` with their `articles`.

The mentioned mutation targets the `author` and `article` tables, involving a nested
insert of article into the author model.

```graphql
mutation insertAuthorWithArticles($name: String, $email:String, $articles_content:[article_insert_input!]!) {
  insert_author(objects: [{name: $name, email: $email, articles: {data: $articles_content}}]){
    returning {
      name
      email
      articles {
        id
      }
    }
  }
}
```

The arguments used in `author` model i.e. `$name`, `$email` and `$articles_content`
(relationship arguments are also considered part of model arguments)  are sent to the
`validate_input` url of `author` model.

The arguments used in `article` model i.e. `$articles_content` is sent to the
`validate_input` url of `article` model.

If both handlers return success, then the mutation proceeds else the error(s) from
the url(s) is forwarded.

The sample payload the validation URL of `author` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
        "input": [
            {
                "name": "Jane",
                "email":"jane@b.com",
                "articles": {
                  "data":{
                    "id":123
                  }
                }
            },
            {
                "name": "Doe",
                "email":"doe@b.com",
                "articles": {
                  "data":{
                    "id":123
                  }
                }
            }
        ]
    }
}

```

The sample payload the validation URL of `article` model receives is:

```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
        "input": [
            {
                "id": 123,
            },
            {
                "id": 345
            }
        ]
    }
}
```
## Update Mutations

### Behaviour

When an update mutation comes in with a role, the following steps are performed:

1. If there is a `validate_input` permission on a table, then any update arguments
   targeting that table are sent to the `url`.
2. If the handlers validates the update arguments , then the request proceeds. **A
   transaction with the database will only be started after the validation is over
   and successful.**
3. If any url invalidates the update arguments, then the request aborts. An `error`
   message from the url can also be forwarded to the client.

Consider the following sample mutation query:
```graphql
mutation update_author {
  update_author(where: { id: { _eq: 3 } }, _set: { name: "Jane" }) {
    affected_rows
  }
}
```

The mentioned mutation targets the `author` table and wants to update the value
present in the table. Assuming that the `validate_input` permission is defined for
the table, the validation process unfolds as follows:

- The validation webhook specified for the `author` table is contacted, including the
  update argument.
- If the above webhook calls result in successful validation, a database transaction
  is initiated to update the rows of the respective tables. 

### Webhook specification

### Request

Consider the following sample mutation query:
```graphql
mutation update_author {
  update_author(where: { id: { _eq: 3 } }, _set: { name: "Jane" }) {
    affected_rows
  }
}
```

The user may want to validate the input values in the `where`, `_set`, `_inc` clause
and `pk_columns`. So, the upstream webhook is expected to receive those values in the
payload.

```json
{
    "version": "<version-integer>",
    "role": "<role-name>",
    "session_variables": {
        "x-hasura-user-id": "<session-user-id>",
        "x-hasura-user-name": "<session-user-name>"
    },
    "data": {
      "input": 
        [
          JSON-fied <model_name>_updates!,
          "pk_columns": JSON-fied <model_name>_pk_columns_input! (only included for update_<mode_name>_by_pk)
        ]
    }
}
```
- `version`: An integer version serves to indicate the request format. Whenever a
breaking update occurs in the request payload, the version will be incremented. The
initial version is set to `1`.
- `role`: Hasura session role on which permissions are enforced.
- `session_variables`: Session variables that aid in enforcing permissions. Variable
  names always starts with `x-hasura-*`.
- `data.updates`: List of the multiple updates to run. The structure of this field
will be similar to the JSONified structure of the `<model_name>_updates!` graphql
type. If it is an update mutation by primary key, then it will also contain the
`<model_name>_pk_columns_input!`


### Response

1. Successful Response

The HTTP validation URL should return a `200` status code to represent successful
validation.

```http
200 OK
```

2. Unsuccessful Response

The HTTP validation URL should return a optional JSON object with `400` status
code to represent failed validation. The object should contain `message` field
whose value is a string and this message is forwarded to client.

If no JSON object is returned then no message is forwarded to client.

```http
400 BAD REQUEST

{
    "message": "Phone Number Invalid"
}
```

When an unexpected response format is received, Hasura raises internal exception.

When an unexpected response format is received, Hasura raises internal exception.


### Examples

1. [Single Update Condition] Check if `name` is a valid string


Consider the following sample mutation query:
```graphql
mutation update_author {
  update_author(where: { id: { _eq: 3 } }, _set: { name: "Jane" }) {
    affected_rows
  }
}
```

The arguments used to update `author` model i.e. `where` and `_set` are sent to the
`validate_input` url of `author` update permission. The url can check the `email`
value. If the url returns success, then the mutation proceeds else the error from the
url is forwarded.

The sample payload the validation URL of `author` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
      "input": [
          {
              "where": {"id": {"_eq": 3}},
              "_set": {"name": "Jane"}
          }
      ]
    }
}
```

2. [Multiple Update Condition] Check if the number of update conditions are less than 50

Consider the following sample mutation query:
```graphql
mutation update_many_articles {
  update_article_many (
    updates: [
      {
        where: {rating: {_lte: 1}},
        _set: {is_published: false}
      },
      {
       where: {rating: {_gte: 4}},
        _set: {is_published: true}
      }
    ]
  ) {
    affected_rows
  }
}
```
The arguments used to update `articles` model i.e. the list of update arguments
(`where` and `_set`) are sent to the `validate_input` url of `author` update
permission. The url can check the number of update arguments. If the url returns
success, then the mutation proceeds else the error from the url is forwarded.

The sample payload the validation URL of `article` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
        "input": [
            {
              "where": {"rating": {"_lte": 1}},
              "_set": {"is_published": false}
            },
            {
              "where": {"rating": {"_gte": 4}},
              "_set": {"is_published": true}
            }
        ]
    }
}
```

3. [Update condition by Primary Key] Check if `name` is a valid string

Consider the following sample mutation query:
```graphql
mutation update_author {
  update_author_by_pk(pk_columns: {id: 3}, _set: { name: "Jane" }) {
    affected_rows
  }
}
```

The sample payload the validation URL of `author` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
      "input": [
          {
              "pk_columns": {"id": 3},
              "_set": {"name": "Jane"}
          }
      ]
    }
}
```

## Delete mutations

### Behaviour

When a delete mutation comes in with a role, the following steps are performed:

1. If there is a `validate_input` permission on a table, then any delete arguments
   targeting that table are sent to the `url`.
2. If the handlers validates the delete arguments , then the request proceeds. **A
   transaction with the database will only be started after the validation is over
   and successful.**
3. If any url invalidates the delete arfuments, then the request aborts. An `error`
   message from the url can also be forwarded to the client.

Consider the following sample mutation query:
```graphql
mutation delete_articles {
  delete_article(where: { author: { id: { _eq: 7 } } }) {
    affected_rows
    returning {
      id
    }
  }
}
```

The mentioned mutation targets the `articles` table and wants to delete the rows
present in the table which satisfy the delete condition. Assuming that the
`validate_input` permission is defined for the table, the validation process unfolds
as follows:

- The validation webhook specified for the `author` table is contacted first,
  including the delete arguments.
- If the above webhook calls result in successful validation, a database transaction
  is initiated to delete the rows of the respective tables. 

## Webhook specification

### Request


Consider the following sample mutation query:

```graphql
mutation delete_articles {
  delete_article(where: { author: { id: { _eq: 7 } } }) {
    affected_rows
    returning {
      id
    }
  }
}
```
The user may want to validate the input values in the `where` clause and
`pk_columns`. So, the upstream webhook is expected to receive those values in the
payload.

```json
{
    "version": "<version-integer>",
    "role": "<role-name>",
    "session_variables": {
        "x-hasura-user-id": "<session-user-id>",
        "x-hasura-user-name": "<session-user-name>"
    },
    "data": {
      "input": 
        {
            JSON-fied <model_name>_bool_exp!,
            "pk_columns": JSON-fied <model_name>_pk_columns_input! (only included for delete_<mode_name>_by_pk)
        }
    }
}
```

- `version`: An integer version serves to indicate the request format. Whenever a
breaking update occurs in the request payload, the version will be incremented. The
initial version is set to `1`.
- `role`: Hasura session role on which permissions are enforced.
- `session_variables`: Session variables that aid in enforcing permissions. Variable
  names always starts with `x-hasura-*`.
- `data.delete`: The delete condition. The structure of this field will be similar to
the JSONified structure of the `<model_name>_bool_exp!` graphql type. If it is an
delete mutation by primary key, then it will also contain the
`<model_name>_pk_columns_input!`

### Response

1. Successful Response

The HTTP validation URL should return a `200` status code to represent successful
validation.

```http
200 OK
```

2. Unsucessful Response

The HTTP validation URL should return a optional JSON object with `400` status
code to represent failed validation. The object should contain `message` field
whose value is a string and this message is forwarded to client.

If no JSON object is returned then no message is forwarded to client.

```http
400 BAD REQUEST

{
    "message": "Phone number invalid"
}
```

When an unexpected response format is received, Hasura raises internal exception

## Examples

1. [Delete Condition] Check if `id` is a valid number


Consider the following sample mutation query:

```graphql
mutation delete_articles {
  delete_article(where: { author: { id: { _eq: 7 } } }) {
    affected_rows
    returning {
      id
    }
  }
}
```


The arguments used to delete `article` model i.e. `where` and `_set` are sent to the
`validate_input` url of `author` update permission. The url can check the `email`
value. If the url returns success, then the mutation proceeds else the error from the
url is forwarded.

The sample payload the validation URL of `author` model receives is:
```json
{
    "version": 1,
    "role": "user",
    "session_variables": {
        "x-hasura-role": "user"
    },
    "data": {
      "input": [
          {
              "where": {"id": {"_eq": 3}}
          }
      ]
    }
}
```



## Enhancements

1. Consider `validate_input` at a column-level. The `definition` spec would differ for this.

2. We can also consider `warning` semantics for `validate_input` url where the validation is success but with warnings.
3. Facilitate users to specify a list of fields that could be sent to url for validation. Grants user control over
determining which fields are sent for validation.
```yaml
validate_input:
  type: http
  defintion: <DEFINITION>
  fields:
  - column_1
  - column_2
  - relationship_1
  - relationship_2
```