# Input Validations

## Index

- [Validation for insert mutations](#validation-for-insert-mutations)
- [Behaviour](#behaviour)
  * [Mutation performance](#mutation-performance)
  * [When `check` is defined](#when--check--is-defined)
- [Webhook specification](#webhook-specification)
  * [Request](#request)
  * [Response](#response)
- [Examples](#examples)
- [Alternative approach](#alternative-approach)
- [Updates and Deletes](#updates-and-deletes)
  * [Update mutation](#update-mutation)
  * [Delete mutation](#delete-mutation)
- [Enhancements](#enhancements)

We want to consider model-level validations during mutations i.e. what type of input values are allowed when mutating a model.
As of date, we can already restrict many types of mutations using `check` rule in mutation permissions. But, this is dependent
on the type of operators that is available for a given model. We want to give an ability to hook an external (or internal) function
for the purposes of validation.

For the first version, we will consider only `insert` mutations for tables.

## Validation for insert mutations

A new field called `validate_input` has been introduced in the insert permission definition to configure the validation.
Find a sample configuration below.

```yaml
type: pg_create_insert_permission
args:
  table: article,
  source: default,
  role: user,
  permission:
    check:
      author_id: "X-HASURA-USER-ID"
    set
      id: "X-HASURA-USER-ID"
    columns: ["name","author_id"]
    validate_input:
      type: http
      definition:
        handler: http://www.somedomain.com/validateArticle
        headers:
        - name: X-Handler-API-Key
          value_from_env: VALIDATION_HOOK_API_KEY
        forward_client_headers: true
        timeout: 5
```

The `type` determines the interface for the input validation, which initially only supports `http` webhook handler.
However, we may expand support for multiple interfaces such as a Postgres function or a remote schema field.
The `definition` field provides necessary context for communicating and submitting the data for input validation. It
is an object with the following fields.
- `handler` - Requred, a [string value](https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl) which supports templating environment variables.
- `headers` - Optional, List of defined headers to be sent to the handler.
- `forward_client_headers` - Optional and default is `false`, If set to `true` the client headers are forwarded to the handler.
- `timeout` - Optional and default is `10`, number of seconds to wait for response before timing out.

## Behaviour

When an insert mutation comes in with a role, the following steps are performed:

1. First "collect" all the tables that the mutation targets (because there could be more than one table involved via nested inserts)
1. If there is a `validate_input` permission on a table, then any arguments targeting that table are sent to the `handler`. This is done for all tables.
1. If all handlers validates the insert data, then the request proceeds. A transaction with the database will only be started after the validation is over and successful.
1. If any handler invalidates the insert data, then the request aborts. An `error` message from the handler can also be forwarded to the client.

Consider the following sample mutation:
```graphql
mutation insertAuthorWithArticles($name: String, $email:String, $articles_content:[article_insert_input!]!) {
  insert_author(objects: {name: $name, email: $email, articles: {data: $articles_content}}){
    returning {
      id
      articles {
        id
      }
    }
  }
}
```
The mentioned mutation targets the `author` and `article` tables, involving a nested insert into the author model.
Assuming that the `validate_input` permission is defined for both tables, the validation process unfolds as follows:

- The validation webhook specified for the `author` table is contacted first, including the inserted row with `articles` data.
- Subsequently, the validation webhook designated for the article table is contacted with `$articles_content` rows.
- If both of the above webhook calls result in successful validation, a database transaction is initiated to insert the rows into the respective tables.

### Mutation performance

Mutations that involve input validation may exhibit slower performance compared to mutations without validation.
The execution time of the webhook handler can become a bottleneck, potentially reaching the maximum limit specified
by the `timeout` configuration value.

### When `check` is defined

A `check` permission is a boolean expression that must be true for every inserted row. This expression is evaluated within the database
during a transaction. When both `validate_input` and `check` permissions are defined, as mentioned earlier, the validation process occurs
first through an external handler. After successful validation, a database transaction is initiated, and the check expression is evaluated.

## Webhook specification

### Request

When an insert mutation on a table with `validate_input` configuration is executed, before making a database transaction Hasura sends the insert data to
the validation HTTP webhook using a `POST` request.

The request payload is of the format:
```json
{
    "version": "<version-integer>"
    "role": "<role-name>",
    "session_variables": {
        "x-hasura-user-id": "<session-user-id>",
        "x-hasura-user-name": "<session-user-name>"
    },
    "data": {
        "objects": [
            {"column_1": "column_1_value", "column_2": "column_2_value", "relationship": [{"relationship_column_1": "column_value"}]},
            {"column_1": "column_1_value", "column_2": "column_2_value", "relationship": [{"relationship_column_1": "column_value"}]}
        ]
    }
}
```
- `version`: An integer version serves to indicate the request format. Whenever a breaking update occurs in the request
payload, the version will be incremented. The initial version is set to `1`.
- `role`: Hasura session role on which permissions are enforced.
- `session_variables`: Session variables that aid in enforcing permissions. Variable names always starts with `x-hasura-*`.
- `data.objects`: List of rows to be inserted which are specified in the `objects` input field of insert mutation. Also includes nested data of relationships.

### Response

The HTTP handler should always return the JSON object with `200` status code. The object should contain `is_valid` field
whose value is a boolean and an optional `error` field with a message forwarded to client when `is_valid` is `false`.

```http
200 OK

{
    "is_valid": true
}
```
or
```http
200 OK

{
    "is_valid": false,
    "error": "Phone number is invalid"
}
```

When response with status other than `200` is received, Hasura raises internal exception.

## Examples

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

The arguments used in `users` model i.e. `$email` and `$name` are sent to the `validate_input` handler of `users` insert permission.
The handler can check the `email` value. If the handler returns success, then the mutation proceeds else the error from the handler is forwarded.

2. [Multiple models] Check if `article` length is less than 1000 when inserting an `author` with their `articles`.

```graphql
mutation insertAuthorWithArticles($name: String, $email:String, $articles_content:[article_insert_input!]!) {
  insert_author(objects: {name: $name, email: $email, articles: {data: $articles_content}}){
    returning {
      id
      articles {
        id
      }
    }
  }
}

```

The arguments used in `author` model i.e. `$name`, `$email` and `$articles_content` (relationship arguments are also considered part of model arguments)  are sent to the `validate_input` handler of `author` model.

The arguments used in `article` model i.e. `$articles_content` is sent to the `validate_input` handler of `article` model.

If both handlers return success, then the mutation proceeds else the error(s) from the handler(s) is forwarded.

## Alternative approach

The `validate_input` as a new expression in the `check` clause of `Insert` permission and can be clubbed with other check permissions.
Find a sample configuration below.

```yaml
type: pg_create_insert_permission
args:
  table: article,
  source: default,
  role: user,
  permission:
    check:
      author_id: "X-HASURA-USER-ID"
      _validate_input:
         type: http
         definition:
           handler: http://www.somedomain.com/validateArticle
    set
      id: "X-HASURA-USER-ID"
    columns: ["name","author_id"]
```
The `check` expressions use boolean syntax and support both `AND` and `OR` operations. However, combining validation
logic with the check expressions can make it difficult for users to understand and maintain. Users mental model may not
be clear when working with input validation. It is easy to comprehend that validation always happens first when it is
isolated from the `check`.


## Updates and Deletes

Let's discuss extending the scope of input validations to update and delete mutations.

### Update mutation

Consider the following sample mutation query:
```graphql
mutation update_author {
  update_author(where: { id: { _eq: 3 } }, _set: { name: "Jane" }) {
    affected_rows
  }
}
```

The user may want to validate the input values in the `where` and `_set` clause. So, the upstream webhook is expected to receive those
values in the payload.

```json
{
    "role": "<role_name>",
    "data": {
        "set": {
            "name": "Jane"
        },
        "where": {
            "id": {
                "_eq": 3
            }
        }
    }
}
```

The `data` field contains necessary information for validation such as
- `where`: a json-ised boolean expression
- `set`: input data to be updated in the table

Also, if provided, json-ised data of other update operators such as
- `_inc` using `inc` field
- `_append` using `append` field
- `_prepend` using `prepend` field
- `_delete_key` using `delete_key` field
- `_delete_at_path` using `delete_at_path` field

are sent in the payload.

Configuration in update permission for http webhook is similar to that of insert permission. Find a sample below.

```yaml
type: pg_create_update_permission
args:
  table: article,
  source: default,
  role: user,
  permission:
    check:
      author_id: "X-HASURA-USER-ID"
    filter:
      author_id: "X-HASURA-USER-ID"
    set:
      updated_at: "NOW()"
    columns: ["name","author_id"]
    validate_input:
      type: http
      definition:
        handler: http://www.somedomain.com/validateUpdateArticle
        headers:
        - name: X-Handler-API-Key
          value_from_env: VALIDATION_HOOK_API_KEY
```

### Delete mutation

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

The delete mutation allows only `where` input field. Hence, only json-ised boolean expression is sent in the payload.

```json
{
    "role": "<role_name>",
    "data": {
        "where": {
            "id": {
                "_eq": 7
            }
        }
    }
}
```

Configuration in delete permission for http webhook is similar to that of insert permission. Find a sample below.

```yaml
type: pg_create_delete_permission
args:
  table: article,
  source: default,
  role: user,
  permission:
    filter:
      author_id: "X-HASURA-USER-ID"
    validate_input:
      type: http
      definition:
        handler: http://www.somedomain.com/validateDeleteArticle
        headers:
        - name: X-Handler-API-Key
          value_from_env: VALIDATION_HOOK_API_KEY
```

## Enhancements

1. Consider `validate_input` at a column-level. The `definition` spec would differ for this.
2. We can also consider `warning` semantics for `validate_input` handler where the validation is success but with warnings.
3. Facilitate users to specify a list of fields that could be sent to handler for validation. Grants user control over
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
