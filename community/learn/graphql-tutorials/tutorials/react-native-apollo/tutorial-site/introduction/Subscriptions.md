# Realtime APIs with GraphQL subscriptions

The GraphQL specification allows for something called subscriptions that are essentially like GraphQL queries but instead of giving you data in one read, you get data pushed from the server, as it happens.

This makes building realtime apps easy, because GraphQL clients support subscriptions and handle the irritating work of dealing with websockets underneath.

## Make your first GraphQL subscription

1. Step 1: Head to https://graphql-tutorials.com/graphiql
2. Step 2: Write this GraphQL query in the textarea:

```
subscription {
  onlineUsers {
    count
  }
}
```

3. Every time the number of onlineUsers change, you'll see a new response on the response window.

## How do GraphQL subscriptions work?

GraphQL queries and mutations are strings sent to a POST endpoint. What is a GraphQL subscription? That can't happen over a POST endpoint, because a simple HTTP endpoint would just return the response and the connection would close.

A GraphQL subscription is a subscription query string sent to a websocket endpoint. And whenever data changes on the backend, new data is pushed over websockets from the server to the client.

---

Next, let's look at the UI app that we're going to integrate a GraphQL API with!
