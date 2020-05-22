### Insert permissions and actions

If an action is used for data validation, it is hard to re-use constraints
specified through insert permissions.

#### Motivation

Let's consider a table with the following schema:

```sql
create table message (
  id serial primary key,
  content text not null,
  channel_id integer not null references channel(id)
)
create table channel (..)
create table channel_members (..)
```

Let's say you want to allow a `user` role to insert into
`message` table if the following `check` constraint holds:
```json
{"channel": {"members": {"user_id": "x-hasura-user-id"}}}
```
i.e, a user can only post a message to a channel if they are a member of that
particular channel. Now let's say you want to do some sort of validation on the
content of the message (maybe through some external service), this is a problem
that would broadly fall under the 'data validation' category (data cannot be
validated with graphql-engine's permissions or through Postgres's check
constraints).

We recommend actions in such cases, we would suggest the developer to remove
the permission for `message` table on `user` and then do the entire validation
in the webhook for the action. However the webhook now has to do more than
validation of the message's content - it also has to check this constraint
`{"channel": {"members": {"user_id": "x-hasura-user-id"}}}` which was part of
insert permissions earlier. This is inconvenient and the reusability of
hasura's permission system is lost.

Why should the insert permission be removed? Because if the insert permission
is defined, the `insert_message` mutation is generated which can be directly
used by anyone with the `user` role and thus bypassing the validation of the
`message` content.

This problem is originally reported
[on discord](https://discordapp.com/channels/407792526867693568/516590536875048990/685462912420282546).

#### Proposed solution

Introduce a new field in insert permission called `admin_only`, this
will remove the insert mutation field for the table from `mutation_root` for
that role but the mutation can still be accessed when `admin-secret` is
specified. The action's handler can now specify the `admin-secret`, set
`x-hasura-role` as the user's role and execute the request so that the
constraints defined on the role in insert permissions are enforced.

Currently `admin-secret` + `x-hasura-role` is used in GraphiQL to mimic a
request as a particular role. So `admin_only` insert mutations will be allowed
when a request is made through GraphiQL with `admin-secret` + `x-hasura-role`.
We'll need to make this clear in our documentation.







