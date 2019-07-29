---
title: "Architecture"
order: 1
metaTitle: "GraphQL Architecture | GraphQL Flutter Tutorial"
metaDescription: "Learn about the architecture of GraphQL, GraphQL over HTTP, the client server model with an example of http request"
---

Before going further in understanding GraphQL, it's useful to get a sense of how
GraphQL is actually used in an HTTP client (typically a web/mobile app).

## GraphQL over HTTP
Check out the diagram below, to get a sense of how GraphQL is typically used in
your stack:

![GraphQL over HTTP](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphql-on-http.png)

### GraphQL client-server flow:

1. Note that the GraphQL query is not really JSON; it looks like the shape of the
   JSON you *want*. So when we make a 'POST' request to send our GraphQL query to
   the server, it is sent as a "string" by the client.
2. The server gets the JSON object and extracts the query string. As per the
   GraphQL syntax and the graph data model (GraphQL schema), the server processes
   and validates the GraphQL query.
3. Just like a typical API server, the GraphQL API server then makes calls to a
   database or other services to fetch the data that the client requested.
4. The server then takes the data and returns it to the client in a JSON object.

### Example GraphQL client setup:

In your day to day work, you don't actually need to worry about the underlying
HTTP requests & responses.

Just like when you work with a REST API and use a HTTP
client to reduce the boilerplate in making API calls and handling responses, you
can choose a GraphQL client to make writing GraphQL queries, sending them and
handling responses much easier.

In fact, the mechanism of how you send the GraphQL query and accept the GraphQL
response has become standard. This makes working with GraphQL very easy on the
client.

Here's what a typical GraphQL client setup and making a query would look like:

```javascript

// Setup a GraphQL client to use the endpoint

const client = new client("https://myapi.com/graphql");


// Now, send your query as a string (Note that ` is used to create a multi-line
// string in javascript).

client.query(`
  query {
    user {
      id
      name
    }
  }`);
```
