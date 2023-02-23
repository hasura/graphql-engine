# Input Validations

We want to consider model-level validations during mutations i.e. what type of input values are allowed when mutating a model. As of date, we can already restrict many types of mutations using `check` rule in mutation permissions. But, this is dependent on the type of operators that is available for a given model. We want to give an ability to hook an external (or internal) function for the purposes of validation.

For the first version, we will consider only `insert` mutations for tables.


## Validation for `insert` mutations

We will consider the following 2 options:

### `validate_input` as a new permission type in `Insert` permissions.

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
      function: http://www.somedomain.com/validateArticle
```

#### Pros

1. Explicit validation permission which makes the execution clear i.e. validation will happen first and then the rest of the execution

#### Cons

1. Cannot combine with other `check` permissions


### `validate_input` as a new expression in the `check` clause of `Insert` 

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
         function: http://www.somedomain.com/validateArticle
    set
      id: "X-HASURA-USER-ID"
    columns: ["name","author_id"]
```

#### Pros

1. Can be combined with other check permissions

#### Cons

1. Mental model is slightly more involved since execution is not explicit.

## Definition

`validate_input` or `_validate_input` (used interchangeably unless explicitly differentiated) is an object with 2 fields: `type` and `function`.

For simplicity, let's say `type` can only take the value `http` and `function` will expect a HTTP URL.

The spec of the `function` is TBD.


## Behaviour

When an insert mutation comes in with a role, the following steps are performed:

1. First "collect" all the tables that the mutation targets (because there could be more than one table involved via nested inserts)
1. If there is a `validate_input` permission on a table, then any arguments targeting that table are sent to the `function`. This is done for all tables.
1. If all functions return success (HTTP status 200), then the request proceeds. A transaction with the database will only be started after the validation is over.
1. If any function returns error (HTTP status 4xx), then the request aborts. An `error` message from the function can also be forwarded to the client.

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

The arguments used in `users` model i.e. `$email` and `$name` are sent to the `validate_input` function of `users` insert permission. 
The function can check the `email` value. If the function returns success, then the mutation proceeds else the error from the function is forwarded.

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

The arguments used in `author` model i.e. `$name`, `$email` and `$articles_content` (relationship arguments are also considered part of model arguments)  are sent to the `validate_input` function of `author` model.

The arguments used in `article` model i.e. `$articles_content` is sent to the `validate_input` function of `article` model.

If both functions return success, then the mutation proceeds else the error(s) from the function(s) is forwarded.

## Enhancements

1. Consider `_validate_input` at a column-level. The `function` spec would differ for this.
2. We can also consider `warning` semantics for `validate_input` function where the validation is success but with warnings.
