---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL React Apollo Typescript Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using React Apollo Client by installing dependencies like apollo-link-ws, subscriptions-transport-ws. This will also have authorization token setup"
---

import GithubLink from "../../src/GithubLink.js";

When we had initially set up Apollo, we installed the required dependencies for a http client. But subscriptions is an advanced use case with websockets. We have to install more dependencies to set up subscriptions.

### React Apollo Subscriptions Setup

```bash
+ $ yarn add apollo-link-ws subscriptions-transport-ws
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/components/App.tsx` and update the following imports:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/App.tsx" text="src/components/App.tsx" />

```javascript
  import ApolloClient from 'apollo-client';
  import { InMemoryCache } from 'apollo-cache-inmemory';
- import { HttpLink } from 'apollo-link-http';
+ import { WebSocketLink } from 'apollo-link-ws';
  import { ApolloProvider } from '@apollo/react-hooks';
```

Update the createApolloClient function to integrate WebSocketLink.

```javascript
const createApolloClient = (authToken: string) => {
  return new ApolloClient({
-   link: new HttpLink({
+   link: new WebSocketLink({
-     uri: 'https://learn.hasura.io/graphql',
+     uri: 'wss://learn.hasura.io/graphql',
+     options: {
+       reconnect: true,
+       connectionParams: {
          headers: {
            Authorization: `Bearer ${authToken}`
          }
+       }
+     }
    }),
    cache: new InMemoryCache(),
  });
};
```

Note that we are replacing HttpLink with WebSocketLink and hence all GraphQL queries go through a single websocket connection. This means not just subscriptions, but also GraphQL queries and mutations also go through websockets.
