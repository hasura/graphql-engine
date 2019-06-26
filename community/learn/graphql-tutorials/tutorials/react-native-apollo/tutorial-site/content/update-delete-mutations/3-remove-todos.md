---
title: "Remove todos - mutation"
metaTitle: "Mutation to delete todos | GraphQL React Native Apollo Tutorial"
metaDescription: "GraphQL Mutation to delete existing personal todos. Try the mutation in GraphiQL, passing the Authorization token to delete a todo"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/B8gdqKbihQI" />

In this part of the tutorial, you will learn how to remove existing todos by using GraphQL Mutations.

Let's define a graphql query to do a mutation into todos.

```graphql
mutation ($id: Int) {
  delete_todos (
    where: {
      id: {
        _eq: $id
      }
    }
  ) {
    affected_rows
  }
}
```

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this graphql mutation into our react app.