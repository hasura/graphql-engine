---
title: "Remove todos - mutation"
metaTitle: "Mutation to delete todos | GraphQL iOS Apollo Tutorial"
metaDescription: "GraphQL Mutation to delete existing personal todos. Try the mutation in GraphiQL, passing the Authorization token to delete a todo"
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

[Try](https://learn.hasura.io/graphql/graphiql) this mutation in GraphiQL against the application database to see what the response looks like. You will also need to pass in the values for the variables.

Let's now integrate this graphql mutation into our ios app.