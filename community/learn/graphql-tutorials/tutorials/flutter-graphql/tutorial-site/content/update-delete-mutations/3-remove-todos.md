---
title: "Remove todos - Mutation"
metaTitle: "Mutation to delete todos | GraphQL Flutter Tutorial"
metaDescription: "GraphQL Mutation to delete existing personal todos. Try the mutation in GraphiQL, passing the Authorization token to delete a todo"
---

In this part of the tutorial, you will learn how to remove existing todos by using GraphQL Mutations.

Let's define a GraphQL query to do a mutation in todos.

```graphql
mutation delete($id:Int!) {
 action: delete_todos(where: {id: {_eq: $id}}) {
    returning {
      id
    }
  }
}
```

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this GraphQL mutation into our Flutter app.