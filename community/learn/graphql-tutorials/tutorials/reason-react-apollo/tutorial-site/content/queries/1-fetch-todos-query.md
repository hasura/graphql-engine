---
title: "Fetch todos - query"
metaTitle: "Query to fetch todo | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "GraphQL Query to fetch personal todos. Try the query in GraphiQL, passing the Authorization token to get authenticated results"
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

The first graphql query you will write will be to fetch personal todos. You will need to load the todo data from the database which belongs to the logged in user. Let's define a graphql query to fetch the required data.

```graphql
query getMyTodos {
  todos(where: { is_public: { _eq: false} }, order_by: { id: desc }) {
    id
    title
    is_completed
    is_public
  }
}
```

[Try](https://learn.hasura.io/graphql/graphiql) this query in GraphiQL against the application database to see what the response looks like.

**Note**: You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token is auto-filled in the UI after logging in via Auth0.

This query is the actual graphql query that we will be using in our ReasonReact app and hence test this out to make sure it works as expected.

Let's now integrate this graphql query into our ReasonReact app.