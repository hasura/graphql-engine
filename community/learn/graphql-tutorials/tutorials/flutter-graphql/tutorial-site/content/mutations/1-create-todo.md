---
title: "Create todos - mutation"
metaTitle: "Mutation to create todos | GraphQL Flutter Tutorial"
metaDescription: "GraphQL Mutation to create new personal todos. Try the mutation in GraphiQL, passing the Authorization token to get authenticated results."
---
In this part of the tutorial, you will learn how to create new todos by using GraphQL Mutations.

Let's define a GraphQL query to do a mutation into todos.

```graphql
  mutation addTodo($title: String!, $isPublic: Boolean!) {
  action: insert_todos(objects: { title: $title, is_public: $isPublic }) {
    returning {
      id
      title
      is_completed
    }
  }
}
```

You will also need to pass in the values for the variables.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this GraphQL mutation into our flutter app.

