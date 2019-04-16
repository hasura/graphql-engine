---
title: "Remove todos - mutation"
---

In this part of the tutorial, you will learn how to remove existing todos by using GraphQL Mutations.

Let's define a graphql query to do a mutation into todos.

```graphql
mutation removeTodo ($id: Int!) {
  delete_todos(where: {id: {_eq: $id}}) {
    affected_rows
  }
}
```

[Try](https://graphiql-online.com) this mutation in GraphiQL against the application database to see what the response looks like.

**Note** You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token can be obtained from the UI; (appears at the top header after logging in)

Let's now integrate this graphql mutation into our react app.