original issue: https://github.com/hasura/graphql-engine/pull/4110

## Allow disabling query root fields

Currently when a select permission is defined for a role on a table, we
automatically generate 3 fields for the table (`<table>`, `<table_by_pk>`,
`<table_aggregate>`) in `query_root` and likewise in `subscription_root`.  This
should be customisable to allow some of the patterns as discussed below.

### Motivation

#### 1. Allow selecting data only through relationships (issues: [207](https://github.com/hasura/graphql-engine/issues/207), [696](https://github.com/hasura/graphql-engine/issues/696), [3742](https://github.com/hasura/graphql-engine/issues/3742)).

Let's say you have a slack like application with the following schema:

| table | columns | relationships |
|-------|---------|---------------|
| workspace | id, name | members(array, to workspace_membership) |
| workspace_membership | workspace_id, user_id |
| channel | id, name, workspace_id | workspace(object, to workspace) |
| message | id, content, user_id, channel_id | channel(object, to channel) |

The permissions for a `user` role would be something along these lines:

| table | permissions |
|-------|-------------|
| workspace | `{"members": {"user_id": "x-hasura-user-id"}}` |
| channel | `{"workspace": {"members": {"user_id": "x-hasura-user-id"}}}` |
| message | `{"channel": {"workspace": {"members": {"user_id": "x-hasura-user-id"}}}}` |

Now let's say we would like to introduce a new table called `message_reaction`
which has columns (message_id, reaciton_name, user_id). The permission on
`message_reaction` table would be as follows:

```json
{"message": {"channel": {"workspace": {"members": {"user_id": "x-hasura-user-id"}}}}}
```

As we go down the chain, our permissions gets more and more nested, refering to
the permissions of the parent tables and beyond a point can get quite
cumbersome. Let's say in our application we **never** need to access
`message_reactions` table directly and is always accessed through `reactions`
relationship on `message` table. Can the permission be simplified?

Yes! *If we can disable all of the `message_reaction` table's top level
fields*, the select filter on `message_reactions` table can be simplified to
`{}` and as `message` table has the correct permissions, the relationship
`reactions` is restricted to what can be accessed through `message` table.

The pattern where certain data can only be accessible through relationships
seems to be known as 'Aggregate' pattern under [Domain-Driven
Design](https://martinfowler.com/bliki/DDD_Aggregate.html).

#### 2. As an additional access control mechanism

Let's say you want to allow a client to fetch data from a table only if the
client knows the primary key of a row in that table. In this case regardless of
the permission on the table, only `<table>_by_pk` should be exposed in
`query_root`.

## Allow disabling subscription fields

Currently we do not provide a fine grained control on subscriptions that are exposed - if a select permission is defined on a table, the live queries on that table are exposed through `subscription_root`. (Note: the discussion of `query_root` customisability also applies to `subscription_root`).

### Proposed solution

Introduce optional `query_root_fields` and `subscription_root_fields` in select permission which takes a list of `field`s that should be exposed in `query_root` (where `field` is one of `select`/`select_by_pk`/`select_aggregate`) and `subscription_root` (`query_root` fields + `select_stream`) respectively. When these fields are absent, all the values are enabled. The current behaviour is for backwards compatibility.
Note: The Relay field `<table>_connection` will be enabled if `select` is given in `query_root_fields` else it will be disabled.

### Metadata API behaviour

For incremental metadata API (`create_select_permission`), throw validation error when:
    a. A role doesn't have access to the primary key column(s) and `select_by_pk` is added.
    b. When `select_stream` is added when streaming subscriptions is not enabled in the graphql-engine.
    c. When `select_aggregate` is added without `allow_aggregations` set to `true`.

For `replace_metadata` API, throw validation error in the above cases when `allow_inconsistent_metadata: false` else mark invalid permissions as inconsistent objects.

### Future work

1. Extend this feature for mutations and remote schemas.
