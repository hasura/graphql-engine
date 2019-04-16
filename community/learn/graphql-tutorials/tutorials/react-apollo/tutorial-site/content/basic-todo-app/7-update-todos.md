---
title: "Update todos - mutation"
---

In this part of the tutorial, you will learn how to mark an existing todo as completed by using GraphQL Mutations.

Let's define a graphql query to do a mutation into todos.

```graphql
  mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
    update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
      affected_rows
    }
  }
```
You will also need to pass in the values for the variables.

[Try](https://graphiql-online.com) this mutation in GraphiQL against the application database to see what the response looks like. 

**Note** You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token can be obtained from the UI; (appears at the top header after logging in)

Let's now integrate this graphql mutation into our react app.
