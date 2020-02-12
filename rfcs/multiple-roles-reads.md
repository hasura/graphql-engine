## Multiple roles (for reads)

Currently, each graphql operation is run with a single role that is provided in the `X-Hasura-Role` session variable. 
This is a RFC to allow a list of roles to be provided (`X-Hasura-Roles`) for query/subscription operations. We will keep mutations out of scope for this RFC.

Original issue: [#877](https://github.com/hasura/graphql-engine/issues/877)

### Motivation

Sometimes permission rules for an operation cannot be satisfactorily expressed with a single role because:

  i) it is complex or hard to maintain,

  ii) it is impossible to create rules because some data is not present in the db,

  iii) you may want to fetch data with different permissions together.

Examples of the above are as follows:

i) **Complex permissions**:

Suppose you are building a forum application like *Reddit*. You might have the following schema:

- Tables:

`posts (id, title, user_id, group_id, published`. A table for posts.

`groups (id, name)`. A table for groups (like _subreddits_).

`user_group_roles(user_id, group_id, role)`. A table where mapping of users with their group-level roles are defined. E.g. a row in `user_group_roles` like (1, 189, "moderator") would mean user with `user_id: 1` has "moderator" level access on group with `group_id: 189`.

- Relationships:

Each `post` has an object relationship with `groups`.

Each `group` has an array relationship with `user_group_roles`.

For this application, you might create a role **user** with select rules on table `posts` like so:

```yaml
_or:
- published:                   # All posts that are published
    _eq: true            
- user_id:                     # All posts that belong to me
    _eq: X-Hasura-User-Id
- group:                       # All posts that belong to the group that I am a moderator of
    user_group_roles:
      _and:
      - role: moderator
      - user_id: X-Hasura-User-Id
```

This is a simple example but it is easy to see that for a more complex application defining all role accesses into one rule will be hard to manage. 

A single role approach like above also make your permissions less *DRY*. For e.g. suppose you want to add a new role in your application called **editor** with select rules as follows:

```yaml
_or:
- published:                   # All posts that are published
    _eq: true            
- user_id:                     # All posts that belong to me
    _eq: X-Hasura-User-Id
```
Now if I have to change the access rule for my application for say all published posts, I would have to do this for both **user** and **editor** roles.

ii) **Data not present in same postgres**:

Suppose you have a table:

`books (id, title, content, author_id, publisher_id)`

but `publisher_id` above does not have any related data in the same postgres (say, it is present in some other source).

You want a user to have read access to all rows for which they are the author or publisher. With single role, you might have a role **user** which uses two attributes `X-Hasura-Author-Id` and `X-Hasura-Publisher-Id` like this:

```yaml
_or:
- author_id:
    _eq: X-Hasura-Author-Id
- publisher_id:
    _eq: X-Hasura-Publisher-Id

```

Now in your auth system, if a user only is an author then they will not be given the `X-Hasura-Publisher-Id` session variable and the rule cannot be resolved.
You can work around this by giving stub values (like -1) for non available attributes but it is clunky.

iii) **Fetch data with different permissions together**:

Suppose you have two tables, `author` and `publisher`. You want to give the role **author** access to table `author` and the role **publisher** access to table `publisher`. 
Now if a user has both **author** and **publisher** roles, you will need to do two queries (one with each role) to fetch all your data. You cannot do the following single query:

```
query {
  author {
    author_id
  }
  publisher {
    publisher_id
  }
} 
```

### Proposal

Authz should handle a `X-Hasura-Roles` session variable which expects a list of roles. E.g. `X-Hasura-Roles: [“editor”, “publisher”]`.

The semantics of `select` permissions are "additive" in nature and would correspond to the following:

- Rows are selected by `OR`ing the filter rules of each role.
- Columns are unions of columns for each role.
- Duplicate rows are merged appropriately, with NULLs as values for non-permissive columns. 
- Aggregates will be computed only for roles that allow select aggregate.
- Limit should correspond to `max` of all roles.

### Sample modeling

The problems mentioned in the **Motivation** section above can be easily solved as follows:

i) **Complex permissions**: 

Now you can have 3 roles: **viewer**, **editor** and **moderator**

Role **viewer** has rules:
```yaml
published:
  _eq: true
```

Role **editor** has rules:
```yaml
user_id:
  _eq: X-Hasura-User-Id
```

Role **moderator** has rules:
```yaml
group:
  user_group_roles:
    _and:
    - role: moderator
    - user_id: X-Hasura-User-Id
```

The roles can be used in any combination and are individually simple.

ii) **Data not present in same postgres**:

Now you can have 2 roles: **author** and **publisher**

Role **author** has rules:
```yaml
author_id:
  _eq: X-Hasura-Author-Id
```

Role **publisher** has rules:
```yaml
publisher_id:
  _eq: X-Hasura-Publisher-Id
```

Now, if a user has any role(s) then it should be expected from the auth system to also provide the necessary attributes.

iii) **Fetch data with different permissions together**:

You can fetch data (with different permissions) together in a single operation.

```
query {
  author {
    author_id
  }
  publisher {
    publisher_id
  }
} 
```
