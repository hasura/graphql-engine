---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using React Native Apollo Client by installing dependencies like apollo-link-ws, subscriptions-transport-ws. This will also have authorization token setup"
---

import GithubLink from "../../src/GithubLink.js";

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/O7Wv2RCQvcc" />

When we had initially set up Apollo, we used Apollo Boost to install the required dependencies. But subscriptions is an advanced use case which Apollo Boost does not support. So we have to install more dependencies to set up subscriptions.

### React Native Apollo Subscriptions Setup

```bash
+ $ npm install apollo-link-ws subscriptions-transport-ws --save
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/apollo.js` and update the following imports:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/apollo.js" text="apollo.js" />

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
