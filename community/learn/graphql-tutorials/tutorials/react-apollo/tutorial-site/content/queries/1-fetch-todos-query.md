---
title: "Fetch todos - query"
---

The first graphql query you will write will be to fetch personal todos. You will need to load the todo data from the database which belongs to the logged in user. Let's define a graphql query to fetch the required data.

```graphql
query getMyTodos {
  todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
    id
    title
    created_at
    is_completed
  }
}
```

[Try](https://learn.hasura.io/graphql/graphiql) this query in GraphiQL against the application database to see what the response looks like.

**Note** You need to pass the `Authorization: Bearer <token>` header before querying to get the results. The token can be obtained from the UI; (appears at the top header after logging in)

Don't be surprised to see results being empty. You haven't added any todos yet! This query is just to ensure if everything works as expected.

Let's now integrate this graphql query into our react app.