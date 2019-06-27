---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client GraphQL Setup | GraphQL Elm Tutorial"
metaDescription: "You will learn how to configure Apollo Client in Elm by installing dependencies like react-apollo, apollo-client, apollo-link-http, apollo-cache-inmemory"
---

import GithubLink from "../src/GithubLink.js";

Elm-graphql doesn't have a native websockets client. The only option available as of now is to use apollo client on the javascript side to make a GraphQL subscription query. We will get into how we can achieve it a little later. 

Lets configure our ApolloClient

### Elm Apollo Installation
Let's get started by installing apollo client & peer graphql dependencies:

```bash
$ npm install --save apollo-client apollo-link-http apollo-cache-inmemory apollo-link-http apollo-link-ws subscriptions-transport-ws graphql graphql-tag
```

### Create Apollo Client Instance
Open `src/index.js` and add the following imports at the top:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/elm-graphql/app-final/src/index.js" text="src/index.js" />

```javascript
import './main.css';
import { Elm } from './Main.elm';

/* */
+ import ApolloClient from "apollo-client";
+ import { split } from 'apollo-link';
+ import { HttpLink } from 'apollo-link-http';
+ import { WebSocketLink } from 'apollo-link-ws';
+ import { getMainDefinition } from 'apollo-utilities';
+ import { InMemoryCache } from "apollo-cache-inmemory";
+ 
+ import gql from 'graphql-tag'
+ 
+ // Replace it with your graphql url
+ const GRAPHQL_URI = 'learn.hasura.io/graphql';
+ 
+ const getClient = (token) => {
+   // Create an http link:
+   const httpLink = new HttpLink({
+     uri: `https://${GRAPHQL_URI}`
+   });
+ 
+   // Create a WebSocket link:
+   const wsLink = new WebSocketLink({
+     uri: `wss://${GRAPHQL_URI}`,
+     options: {
+       reconnect: true
+       , connectionParams: {
+         headers: {
+           Authorization: `Bearer ${ token }`
+         }
+       }
+     }
+   });
+ 
+   // using the ability to split links, you can send data to each link
+   // depending on what kind of operation is being sent
+   const link = split(
+     // split based on operation type
+     ({ query }) => {
+       const definition = getMainDefinition(query);
+       return (
+         definition.kind === 'OperationDefinition' &&
+         definition.operation === 'subscription'
+       );
+     },
+     wsLink,
+     httpLink,
+   );
+   const client = new ApolloClient({
+     link: link,
+     cache: new InMemoryCache({
+       addTypename: true
+     })
+   });
+   return client;
+ };

document.addEventListener("DOMContentLoaded", function() {
  var app = Elm.Main.init({
    node: document.getElementById("root")
  });
})

```

Let's try to understand what is happening here. 

### HttpLink and InMemoryCache
We are creating an `HttpLink` and `wsLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate ApolloClient by passing in our link and a new instance of `InMemoryCache` (recommended caching solution). We are wrapping all of this in a function which will return the client.

We are going to make use of this function to initiate GraphQL subscriptions.
