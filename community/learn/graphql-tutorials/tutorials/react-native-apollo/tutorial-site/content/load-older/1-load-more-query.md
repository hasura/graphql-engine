---
title: "GraphQL Query to load older todos"
metaTitle: "GraphQL Query to load older todos | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn the GraphQL query to be made to load older todos with parameters and arguments."
---

import GithubLink from "../../src/GithubLink.js";

Firstly, we need to modify our first `FETCH_TODOS` query such that it fetches only 10 items on first load. To do that, just add the `limit` argument to it. Go to `src/screens/components/Todos` and add the `limit` argument and set it to `10`.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/screens/components/Todo/Todos.js" text="Todos.js" />

```graphql
 query ($isPublic: Boolean) {
   todos (
     order_by: {
       created_at: desc
     },
     where: { is_public: { _eq: $isPublic} },
+    limit: 10
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

Now the above GraphQL query would fetch only the last 10 todos.

Now let us write the query to load older todos:

```graphql
query ($lastId: Int, $isPublic: Boolean){
  todos (
    order_by: {
      id: desc
    },
    where: {
      _and: {
        is_public: { _eq: $isPublic},
        id: { _lt: $lastId}
      }
    },
    limit: 10
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
------------------------

This query fetches all the todos which have `id` greater than the value coming from the variable `$lastId` and which have `is_public` equal to the value of `$isPublic` in variables.

Let us go ahead and integrate this now.
