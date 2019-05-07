---
title: "Subscription"
---

When we had initially set up Apollo, we used Apollo Boost to install the required dependenices. But subscriptions is an advanced use case which Apollo Boost does not support. So we have to install more dependenices to set up subscriptions.

```bash
+ $ npm install apollo-link-ws subscriptions-transport-ws --save
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/apollo.js` and update the following imports:

```javascript
- import { HttpLink } from 'apollo-link-http';
+ import { WebSocketLink } from 'apollo-link-ws';
```

Update the `makeApolloClient` function to integrate WebSocketLink.

```javascript

  // create an apollo link instance, a network interface for apollo client
-  const link = new HttpLink({
-    uri: `https://learn.hasura.io/graphql`,
-    headers: {
-      Authorization: `Bearer ${token}`
-    }
-  });

+ const link = new WebSocketLink({
+   uri: `wss://learn.hasura.io/graphql`,
+   options: {
+     reconnect: true,
+     connectionParams: {
+       headers: {
+         Authorization: `Bearer ${token}`
+       }
+     }
+   }
+ })
```

Note that we are replacing HttpLink with WebSocketLink and hence all GraphQL queries go through a single websocket connection.
