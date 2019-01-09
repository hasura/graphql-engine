# Your first GraphQL query

It's time to try your first GraphQL query out with raw HTTP so that you really understand how things are working:

- Step 1: We have built a GraphQL API for you and hosted it at this endpoint:
- Step 2: Open curl / Postman or your favourite normal HTTP client and make this query:

```
Method: POST
URL: https://backend.graphql-tutorials.com/v1alpha1/graphql
Content-Type: application/json
Body:
{
  "query": "query { user { id name }}"
}
```

- Step 3: Hopefully that was a demystifying experience. But writing queries as strings inside a JSON body is a terrible experience.
- Step 4: GraphiQL, a tool built by the GraphQL community is the ultimate tool working with a GraphQL API. Head to: https://graphql-tutorials.com/graphiql
- Step 5: In the main text area, write this query:

```
query {
  user {
    id
    name
  }
}
```

- Underneath, GraphiQL is doing the same thing you did in Step 2. It's just much nicer with the autocomplete, syntax highlighting and query validation.

## Why are GraphQL queries better than my simple GET endpoint?

3 simple reasons, at this stage in your GraphQL journey:

1. You can query for whatever data you want. Try removing a few fields and making the query and notice that the response only contains what you want.
2. You can fetch related data, or a "graph" in the same query. Try making this query:

```
query {
  user {
    id
    name
    todos {
      id
      title
    }
  }
}
```

3. You don't need docs. Just hit autocomplete to see what fields your API models have. Otherwise, go to the docs tab on the side and browse all the API models in one shot. GraphiQL works with any GraphQL API without API authors needing to write any documentation, and mostly, without you having to read any documentation :)

## Can I only read data with GraphQL?

GraphQL is optimised for making amazing queries to fetch data, but you can also "mutate" data (POST, PUT, PATCH, DELETE in the REST world) or "subscribe" to realtime data.

Next, let's look at writing/updating/deleting data.
