---
title: "Fetch todos - query"
metaTitle: "Query to fetch todo | GraphQL React Apollo Typescript Tutorial"
metaDescription: "GraphQL Query to fetch personal todos. Try the query in GraphiQL, passing the Authorization token to get authenticated results"
---

The first graphql query you will write will be to fetch personal todos. You will need to load the todo data from the database which belongs to the logged in user. Let's define a graphql query to fetch the required data.

```graphql
query getMyTodos {
  todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
    id
    title
    is_completed
  }
}
```

[Try this query in GraphiQL](https://learn.hasura.io/graphql/graphiql) against the application database to see what the response looks like.

**Note**: You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token is auto-filled in the UI after logging in via Auth0.

This query is the actual graphql query that we will be using in our react app and hence test this out to make sure it works as expected.

What does this query do? 
------------------------
The query fetches `todos` with a simple condition; `is_public` must be false. We sort the todos descending by its `created_at` time according to the schema. We specify which fields we need for the todos node.


Let's now integrate this graphql query into our react app.