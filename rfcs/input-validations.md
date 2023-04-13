# Input Validations

We want to consider model-level validations during mutations i.e. what type of input values are allowed when mutating a model. As of date, we can already restrict many types of mutations using `check` rule in mutation permissions. But, this is dependent on the type of operators that is available for a given model. We want to give an ability to hook an external (or internal) function for the purposes of validation.

For the first version, we will consider only `insert` mutations for tables.


## Validation for `insert` mutations

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
```

The `type` determines the interface for the input validation, which initially only supports `http` webhook handler.
However, we may expand support for multiple interfaces such as a Postgres function or a remote schema field.
The `definition` field provides necessary context for communicating and submitting the data for input validation.

### Alternative approach

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

## Behaviour

When an insert mutation comes in with a role, the following steps are performed:

1. First "collect" all the tables that the mutation targets (because there could be more than one table involved via nested inserts)
1. If there is a `validate_input` permission on a table, then any arguments targeting that table are sent to the `handler`. This is done for all tables.
1. If all handlers return success (HTTP status 200), then the request proceeds. A transaction with the database will only be started after the validation is over.
1. If any handler returns error (HTTP status 4xx), then the request aborts. An `error` message from the handler can also be forwarded to the client.

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

## Enhancements

1. Consider `validate_input` at a column-level. The `definition` spec would differ for this.
2. We can also consider `warning` semantics for `validate_input` handler where the validation is success but with warnings.
