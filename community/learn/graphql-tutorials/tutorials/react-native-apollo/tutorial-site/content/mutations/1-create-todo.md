---
title: "Create todos - mutation"
metaTitle: "Mutation to create todos | GraphQL React Native Apollo Tutorial"
metaDescription: "GraphQL Mutation to create new personal todos. Try the mutation in GraphiQL, passing the Authorization token to get authenticated results."
---


import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/wjOBsczz_fU" />

In this part of the tutorial, you will learn how to create new todos by using GraphQL Mutations.

Let's define a graphql query to do a mutation into todos.

```graphql
  mutation ($text: String!, $isPublic: Boolean){
    insert_todos (
      objects: [{
        title: $text,
        is_public: $isPublic
      }]
    ){
      returning {
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
  }
```

You will also need to pass in the values for the variables.

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this mutation in GraphiQL against the application database to see what the response looks like.

Let's now integrate this graphql mutation into our react app.

