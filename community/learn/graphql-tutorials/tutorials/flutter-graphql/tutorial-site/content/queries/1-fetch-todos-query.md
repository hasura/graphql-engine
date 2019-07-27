---
title: "Fetch todos - Query"
metaTitle: "Query to fetch todo | GraphQL Flutter Tutorial"
metaDescription: "GraphQL Query to fetch personal todos. Try the query in GraphiQL, passing the Authorization token to get authenticated results"
---



The first graphql query you will write will be to fetch personal todos. You will need to load the todo data from the database which belongs to the logged-in user. Let's define a graphql query to fetch the required data.

```graphql
 query getMyTodos {
  todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
    __typename
    id
    title
    is_completed
  }
}
```

[Try](https://learn.hasura.io/graphql/graphiql?tutorial=react-native) this query in GraphiQL against the application database to see what the response looks like.

**Note**: You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token is auto-filled in the UI after logging in via Auth0.

Don't be surprised to see results being empty. You haven't added any todos yet! This query is just to ensure if everything works as expected.

Now let's integrate this GraphQL query into our Flutter app.
