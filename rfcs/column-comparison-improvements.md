## Improvements to column comparison expression

In SQL, a boolean expression which involves a comparison operator takes
the following shape.

```sql
{lhs-expression} operator {rhs-expression}
```

In graphql-engine, it takes the following shape:

```
{ "column": { "_operator": RHS } }
```

The syntax that we picked over json limits us to columns on the lhs.

In Postgres, to compare a column against a scalar value, we would write something like this:

```sql
"author_id" = 'some_value'
````

to compare a column against another column, we would write:

```sql
"author_id" = "editor_id"
````

In graphql-engine, to compare against a scalar value, it would be:

```json
{ "author_id": { "_eq": "some_value" } }
```

In Postgres, the `=` operator works on both scalar values and columns as the
rhs type (column or a scalar value) is determined by whether the expression
starts with `'`(scalar value) or `"` (an identifier).

In graphql-engine, we instead determine the type of the RHS by introducing a
new operator for `_eq` called `_ceq` that's meant to compare columns. So we have

```json
{ "author_id": { "_ceq": "editor_id" } }
```

In addition to this, we have a very inelegant way of specifying session variables
in boolean expressions:

```json
{
  "author_id": {
    "_eq": "x-hasura-user-id"
  }
}
```

If the RHS is a json string which starts with `x-hasura` prefix, we then treat it
as a session variable. In hindsight, we could've had this cleaner syntax:

```json
{
  "author_id": {
    "_eq": {
      "value": "scalar value",
      "session_variable": "x-hasura-user-id",
      "column": "editor_id"
    }
  }
}
```

But unfortunately we are stuck with what we have. When in future, if we have to
overhaul our boolean expression syntax, maybe we can come up with something
cleaner. Now coming back to the problem at hand, we have a bunch of column operators
like `_ceq`, `_cne`, `_cgt` etc but they'll all expect a column on which the
expression is being defined on.

```json
{ "author_id": { "_ceq": "editor_id" } }
```

This however is limiting to express certain conditions. Let's consider this schema:

```
user (id, ..)
group (id, ..)
membership (user_id, group_id, join_date)
message (id, group_id, posted_at, flagged, ...)
```

with the following relationships:

```
user:
  groups (array, to membership table)

group:
  members (array, to membership table)

membership:
  group (object, to group table)
  user (object, to user table)

message:
  group (object, to group table)
```

The condition to be expressed is as follows:

> A user can only access unflagged messages of the groups they belong to
**and** only which are posted **after** they have joined the group

The first two conditions are straightforward:

On the `message` table, the `filter` expression would be as follows:

```json
{
  "flagged": {
    "_eq": false
  },
  "group" : {
    "members": {
      "user_id": {
        "_eq": "x-hasura-user-id"
      }
    }
  }
}
```

which reads as follows:
> where flagged = false AND the current user exists in the `group`'s `members`.

However, we can't add the condition with what we have today:

```json
{
  "flagged": {
    "_eq": false
  },
  "group" : {
    "members": {
      "user_id": {
        "_eq": "x-hasura-user-id"
      },
      "join_date": {
        "_cgt": ??
      }
    }
  }
}
```

Let's look at how we would've written the same expression in SQL:

```sql
SELECT
  *
FROM
  messages
WHERE
  NOT flagged
  AND EXISTS (
    SELECT
      1
    FROM
      membership
    WHERE
      group_id = messages.group_id
      AND user_id = CURRENT_USER
      AND join_date > messages.post_date
  )
```

We could reference the `post_date` from `messages` table which is allowed by
SQL scoping rules (loosely speaking, all the tables that are involved in the
joins can be referenced). We can bring in similar functionality to our boolean
expression syntax.

```json
{
  "flagged": {
    "_eq": false
  },
  "group" : {
    "members": {
      "user_id": {
        "_eq": "x-hasura-user-id"
      },
      "join_date": {
        "_cgt": ["$", "post_date"]
      }
    }
  }
}
```

where `$` denotes the table on which this permission is being defined -
`messages` table. So, `["$", "post_date"]` is same as saying
`messages.post_date` in the above SQL expression.

Column operators now take a *path* to a column by traversing relationships. By
default, the path is assumed to start from the table on which the column
expression is defined but we can start from the root table (table on which the
permission is defined) by starting the path with `$`.

A column operator's RHS is defined as follows:
```
ColumnOperatorRHS = ColumnName | ColumnPath
ColumnPath = [ ($,)? {RelationshipName,}* ColumnName ]
```

1. There should atleast be one element in the path.
1. The last element of the path should be a column, all the other elements have
   to be relationship names.
1. The first element can optionally be `$`.

`{ "_column_operator": "column_name" }` is equivalent to `{ "_column_operator":
["column_name"] }`

Let's say a message's `post_date` is present in a separate `message_metadata`
table linked through an object relationship `metadata` on `message` table. In
this case, the permission expression would change as follows:

```json
{
  "flagged": {
    "_eq": false
  },
  "group" : {
    "members": {
      "user_id": {
        "_eq": "x-hasura-user-id"
      },
      "join_date": {
        "_cgt": ["$", "metadata", "post_date"]
      }
    }
  }
}
```

Let's say `group` table has a column `subscription_ends_at`, and a message is
only allowed to be read if it was created when the subscription was valid.

```json
{
  "post_date": {
    "_clt": ["group", "subscription_ends_at"]
  }
}
```
Note the absence of `$` in the path.
