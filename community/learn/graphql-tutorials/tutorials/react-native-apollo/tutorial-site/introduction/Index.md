# Introduction to GraphQL

GraphQL is a specification for how to talk to an API. The key idea is to `POST` a "query" to an HTTP endpoint instead of hitting different HTTP endpoints for different resources.

It is designed for developers of web/mobile apps (HTTP clients) to be able to make API calls to fetch the data they need from their backend APIs in a more convenient way.

Let's say you have an API to fetch a user's profile details:

```
GET /api/user?id=1

----

{
  "id": 1,
  "name": "user1"
}
```

If your API server was a GraphQL server instead, this is what your query could look like:

```
POST /graphql

query {
  user(id: 1) {
    id
    name
  }
}

----

{
  "id": 1,
  "name": "user1"
}
```

Notice a few key points:

- Instead of 'GET'ing a resource you 'POST' a query to describe what you want.
- The data you send is not JSON but some kind of weird language that looks like the "shape" of the JSON response you want.
- This new "language" in which you write your queries is called GraphQL
