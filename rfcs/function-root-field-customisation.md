# Customising Function Root Field Names

## Metadata

```
---
authors:
   Vamshi Surabhi <vamshi@hasura.io>,
   Gil Mizrahi <gil@hasura.io,
   Evie Ciobanu <evie@hasura.io>,
   Philip Lykke Carlsen <philip@hasura.io>
discussion:
   https://github.com/hasura/graphql-engine/issues/7405
state: published
---
```

## Description

We should let users customise the GraphQL name that appears in the schema of SQL
Functions tracked by The GraphQL Engine.

### Problem

Being able to customise the names that _tables_ are exposed under in the GraphQL
schema is already supported by The GraphQL Engine, since
[PR-5719](https://github.com/hasura/graphql-engine/pull/5719).

Citing the above PR for motivation:

> .. The identifier can be useful to use when tracking tables with non GraphQL
> compliant names or it can be useful if the nodes that are generated should not
> be dependent on the table name i.e when a table is tracked with an identifier
> and if the table has been renamed, the node names will not be changed due to
> the identifier.

The same motivation applies for functions that are exposed in the GraphQL Schema.

### Success

This feature will have been implemented fully when a user is able to, through the Console:

* Track a function
* Set a custom name for it
* Use that name in queries

Similarly, the server-specific part will have been implemented once there is an
integration test that performs the above steps via the server API.

## What

The server part should add new metadata API endpoint(s) that allows a user, for
some given tracked function, to specify a custom name that is used to query
this function in the GraphQL Schema. In the absence of this feature, the name of
the tracked function is just directly exposed in the GraphQL schema
<a name="footnote-1-ref"></a>[[1]](#footnote-1-def).

The console part should include a tab within the tracked function page that lets
users specify the customised name, similarly to how custom table names are
specified.

It's important to note that this change only affects the GraphQL frontend,
leaving SQL generation untouched.

Also note that, since the Postgres backend is the only one that supports
functions, the change described in this document is only in scope for Postgres.
In the longer perspective, the query abstraction feature that will be
implemented across backends will not be SQL Functions directly, but rather
_Stored Queries_, which as of this writing have yet to be specified. (See [Slack
Discussion](https://hasurahq.slack.com/archives/C01RZPEPF0W/p1627580823109200))

### Effects and Interactions

This will require work on both Server and Console. Once the Server parts have
been implemented the Console parts can be implemented.

This adds new metadata API endpoints. We don't perceive this feature to
interfere with any other features.

### Unresolved Questions

None. See linked discussion for historically unresolved questions.

## How

Functions already have a `configuration` section - we can add two top level
fields called `custom_name` and `custom_root_fields` as follows:

```yaml
functions:
- function: function_name
  configuration:
    session_argument: ...
    exposed_as: ...
    custom_name: a custom graphql compliant name
    custom_root_fields:
      function: FunctionName
      function_aggregate: SomeOtherName
  permissions: ...
```

Console will require an equivalent of `set_table_customization`, maybe
`set_function_configuration` to allow modifying this configuration after a
function is tracked. 

The `configuration` section of a function (`FunctionMetadata b`) needs two new
options: 

1. `custom_name` is optional and expects a GraphQL name. `custom_name` should be
   used in place of the function's name when generating GraphQL fields or types
   for the function. 

1. `custom_root_fields` is optional and expects an object with the following two
   fields (both should be GraphQL names):

   1. `function`: This name should be used for the field name which exposes the
      function in the appropriate operation root (query/mutation/subscription).
      When this not specified, we use `<custom_name/function_name>` as the field
      name.
   
   1. `function_aggregate`: This name should be used for the aggregate operation
      that is exposed on the function. In the absence of this option, we use
      `<custom_name/function_name>_aggregate` as the field name.
   
   Note that both `function` and `function_name` take precedence over
   `custom_name`. 

We do not expect functions support on other databases, so this is only expected
to work on Postgres databases. 

(These are equivalent to `custom_name` and `custom_root_fields` in a table's
`configuration` section (`TableConfig b`))

In addition, we need an API for updating this configuration from the console.
For table configuration, it is done through `set_table_customization` so we need
something similar to that for functions too. I'm wondering if it makes sense to
just add `replace_if_exists` key to `track_function` API instead of a
`set_function_customization` API.


### Examples
Example, consider the following schema:

```sql
create table article (
  id serial primary key,
  title text not null,
  content text not null
);
create function search_articles(key text)
returns setof articles AS $$
  select * from article
  where
    title ilike ('%' || key || '%')
    OR content ilike ('%' || key || '%')
$$ language sql stable;
```

with this metadata:

```yaml
version: 3
sources:
- name: wb
  kind: postgres
  configuration:
    connection_info:
      database_url:
        from_env: WORKBENCH_URL
  tables:
  - table: article
  functions:
  - function: search_articles
```

We would have the following fields in `query_root` (considering only the fields
of function `search_articles`):

```graphql
search_articles(
  args: search_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): [article!]!
search_articles_aggregate(
  args: search_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): article_aggregate!
```

If `custom_name` is specified as follows:

```yaml
version: 3
sources:
- name: wb
  kind: postgres
  configuration:
    connection_info:
      database_url:
        from_env: WORKBENCH_URL
  tables:
  - table: article
  functions:
  - function: search_articles
    configuration:
      custom_name: find_articles
```

the root fields would have the following schema:

```graphql
find_articles(
  args: find_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): [article!]!
find_articles_aggregate(
  args: find_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): article_aggregate!
```

The changes are:

```
search_articles -> find_articles (root field name)
search_articles_aggregate -> find_articles_aggregate (root field name)
search_articles_args -> find_articles_args (input type name)
```

Instead, if `custom_root_fields` are specified as follows:

```yaml
version: 3
sources:
- name: wb
  kind: postgres
  configuration:
    connection_info:
      database_url:
        from_env: WORKBENCH_URL
  tables:
  - table: article
  functions:
  - function: search_articles
    configuration:
      custom_name: find_articles
      custom_root_fields:
        function: FindArticles
        function_aggregate: FindArticlesAggregate
```

the generated schema should be:

```graphql
FindArticles(
  args: find_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): [article!]!
FindArticlesAggregate(
  args: find_articles_args!
  distinct_on: [article_select_column!]
  limit: Int
  offset: Int
  order_by: [article_order_by!]
  where: article_bool_exp
  ): article_aggregate!
```

The changes are:

```
find_articles -> FindArticles (root field name)
find_articles_aggregate -> FindArticlesAggregate (root field name)
```

## Future work

In general maybe we can extend most of our
`track_*/create_*` APIs with `replace_if_exists` similar to `CREATE OR REPLACE`
in Postgres? 


## Footnotes

<a name="footnote-1-def"></a>[1][^](#footnote-1-ref): Subject to the name being made into a valid GraphQL identifier.
