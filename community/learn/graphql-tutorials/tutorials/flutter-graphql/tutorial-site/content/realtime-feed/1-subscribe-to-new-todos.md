---
title: "Detect new todos"
metaTitle: "Subscribe to new todos | GraphQL Flutter Tutorial"
metaDescription: "You will learn how to make use of GraphQL Subscriptions to get notified whenever a new todo comes in Flutter app"
---

In this section, we will capture newly added public todos in the database. This can be done by subscribing to the last todo added in the database. The subscription query looks like:

```graphql
subscription {
  todos (
    order_by: {
      id: desc
    }
    limit: 1
    where: { is_public: { _eq: true }}
  ) {
    id
  }
}
```

What does this subscription do?
-------------------------------

The above subscription gives only the "last added" element in the database (ordered by `id` and limit set to 1). So whenever a todo is added in the database, this subscription will receive data.

We can treat this subscription data as an event notification saying "something has been added" and fetch all the todos from the database latest with respect to your todo present in the local cache.

Whenever such an event notification occurs, we can fetch the newer todos with the following query:

```graphql
query ($lastId: Int){
  todos (
    order_by: {
      id: desc
    },
    where: {
      _and: {
        is_public: { _eq: true},
        id: { _gt: $lastId}
      }
    }
  ) {
    id
    title
    is_completed
    created_at
    is_public
    user {
      name
    }
  }
}
```

What does this query do?
-----------------------

This query takas a query variable called `$lastId` and fetches all the todos with `id` greater than the value of `$lastId`.

Now let us wire up this functionality in our app.
