---
title: "Update todos - mutation"
metaTitle: "Mutation to update todos | GraphQL Flutter Tutorial"
metaDescription: "GraphQL Mutation to update existing personal todos. Try the mutation in GraphiQL, passing the Authorization token to mark a todo as completed"
---

In this part of the tutorial, you will learn how to mark an existing todo as completed by using GraphQL Mutations.

Let's define a GraphQL query to do a mutation into todos.

```graphql
  mutation toggleTodo($id:Int!, $isCompleted: Boolean!) {
  action: update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
    returning {
      is_completed
    }
  }
}
```

You will also need to pass in the values for the variables.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this GraphQL mutation into our Flutter app.
