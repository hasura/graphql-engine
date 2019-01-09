GraphQL includes reusable units called [fragments](https://graphql.org/learn/queries/#fragments). Fragments let you construct sets of fields, and then include them in queries where you need to.

In our todo app, we made multiple queries/mutations/subscriptions. But a few of them had something in common.

Open `src/components/Todo/TodoQueries.js` and look at the common parts of code.

Most of them require the same set of fields from the todos node.

```
    {
        id
        text
        is_completed
        created_at
        is_public
    }
```

The above set of fields are repeated in different queries. This looks like a lot of code repetition. Let's declare a `fragment` which can be used inside all of these queries.

In `src/components/Todo/TodoQueries.js` file, add the following fragment code at the top just below the import;

```
const TODO_FRAGMENT = gql`
  fragment TodoFragment on todos {
    id
    text
    is_completed
    created_at
    is_public
  }
`;
```

So here, we declare a fragment `TodoFragment` on type `todos` to have the reused fields. Now this fragment can be used across other queries.

For example, `QUERY_PRIVATE_TODO` can be updated to the following:

```
const QUERY_PRIVATE_TODO = gql`
  query fetch_todos($userId: String!) {
    todos(
      where: { is_public: { _eq: false }, user_id: { _eq: $userId } }
      order_by: { created_at: desc }
    ) {
      ...TodoFragment
    }
  }
  ${TODO_FRAGMENT}
`;
```

Note the use of `...TodoFragment`, where the fields we declared in the fragment will be spread.

Similarly let's create a fragment for `user`

```
const USER_FRAGMENT = gql`
  fragment UserFragment on users {
    name
  }
`;
```

Here, we are declaring a fragment `UserFragment` on type `users` with the name field.

This can be now used with `QUERY_PUBLIC_TODO` as following:

```
const QUERY_PUBLIC_TODO = gql`
  query fetch_todos($todoLimit: Int, $todoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $todoId } }
      order_by: { created_at: desc }
      limit: $todoLimit
    ) {
      ...TodoFragment
      user {
        ...UserFragment
      }
    }
  }
  ${TODO_FRAGMENT}
  ${USER_FRAGMENT}
`;
```

**Note** Here we are using multiple fragments in the same query.