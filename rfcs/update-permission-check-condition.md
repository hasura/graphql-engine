## Update permissions: Allow checking a condition on an updated row

Our insert permissions allow checking a condition on an inserted row but
update permissions only allow restricting updates to a set of rows
(with `filter`) - there is no means to check a condition on the updated row.


### Motivation

Consider this schema for a slack like application:

```sql
create table slack_user (
  id serial primary key,
  name text not null
);

create table workspace (
  id serial primary key,
  name text not null
);

create table workspace_membership (
  id serial primary key,
  workspace_id integer references workspace (id),
  user_id integer references slack_user (id),
  user_role text not null
);
```

Let's say a user can have 3 kinds of roles for a workspace, 'admin', 'moderator'
and 'user' (modelled with `role` column in `workspace_membership`). The permissions
for `workspace_membership` table are as follows:
1. If a 'user' is an 'admin' of a workspace, they can add any other user to the
   workspace with any role or modify the membership of any user.
2. If a 'user' is a 'moderator' of a workspace, they can only add other users
   with `moderator` or `user` role and the updates too are restricted to these roles.

The `insert` permission on `workspace_membership` will be as follows:

```json
{
  "_or": [
    {
      "workspace": {
        "members": {
          "user_id": {"_eq": "x-hasura-user-id"},
          "user_role": {"_eq": "admin"}
        }
      }
    },
    {
      "workspace": {
        "members": {
          "user_id": {"_eq": "x-hasura-user-id"},
          "user_role": {"_eq": "moderator"}
        }
      },
      "user_role": {
        "_in": ["user", "moderator"]
      }
    }
  ]
}
```

Let's try specifying an `update` permission on `workspace_membership`:

1. What are the set of rows that can be modified by a user?

   The rows where the user is a 'moderator' or an 'admin' of the workspace. So, it would be:
   ```json
    {
      "workspace": {
        "members": {
          "user_id": {"_eq": "x-hasura-user-id"},
          "user_role": {"_in": ["admin", "moderator"]}
        }
      }
    }
    ```

2. What columns can be updated?

   An admin or a moderator should be able to modify the `user_role` column. However, if we allow
   modifying this column, a moderator can set the `user_role` value to `admin`. So we will also
   need to check a condition (in this case, same as insert's check condition) on the updated
   row.

   This is currently missing, we'll need to add an insert permission's `check` condition
   feature for update permissions too.


### Proposed change

Update permission will have a new field called `"check"` which takes as boolean
condition, similar to insert permission. The semantics will be as follows:

> A row is only updated if the row is allowed to be updated with `filter` and the
updated row holds the condition specified with `check`.

#### Other options considered:

- Why introduce a `check` field in the update permission? Why not just apply the
  insert permission's `check` condition on updates?

  1. It may not make sense to allow inserts, but a check condition on update needs
  to be specified.
  2. The check conditions maybe different for both insert and update permisisons.

### Implementation

In case of update mutations, the `check` condition can be checked the same way as how
insert's check condition is checked, by making it part of `returning`.  The
tricky part would be the behaviour when `on_conflict` is used:

1. When there is no conflict, the insert permission's `check` condition has to
   hold true on the inserted row.
2. When there is a conflict, the update should only happen if the row can be
   updated, i.e, the update permission's `filter` condition holds true **and**
   after the row is updated, the update permission's `check` condition has to
   hold true.

This is pretty much what Postgres does while enforcing [RLS policies](https://www.postgresql.org/docs/current/sql-createpolicy.html).
The relevant parts from the above doc are:

> Note that `INSERT` with `ON CONFLICT DO UPDATE` checks `INSERT` policies'
`WITH CHECK` expressions only for rows appended to the relation by the INSERT path.

> When an `INSERT` command has an auxiliary `ON CONFLICT DO UPDATE` clause,
if the `UPDATE` path is taken, the row to be updated is first checked against
the `USING` expressions of any `UPDATE` policies, and then the new updated row
is checked against the `WITH CHECK` expressions.

`filter` and `check` in our permissions are modelled after `USING` and `CHECK`
in RLS. How do we enforce update permission's `filter` and `check` conditions
without having access to low level interfaces like Postgres does?

1. `filter`: we already do this, by adding the condition to `WHEN` in the
   `INSERT` statement.
2. `check`: not as straight forward, we'll need to know whether the row has
   been inserted or updated so that we evaluate the correct `check` condition in
   `returning`.  This seems possible by checking a
   [system column](https://www.postgresql.org/docs/current/ddl-system-columns.html)
   `xmax` (see [this](https://stackoverflow.com/q/34762732)). So the `returning`
   clause would probably look like:
   ```sql
   returning
     *,
     IF (xmax = 0) THEN (insert's check condition) ELSE (update's check condition)
   ```

