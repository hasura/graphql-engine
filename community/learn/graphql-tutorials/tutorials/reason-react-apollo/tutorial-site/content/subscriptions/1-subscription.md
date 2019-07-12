---
title: "Subscription"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/yZmVWeyoW_4" />

When we had initially set up Apollo, we used Apollo Boost to install the required dependencies. But subscriptions is an advanced use case which Apollo Boost does not support. So we have to install more dependencies to set up subscriptions.

```bash
+ $ npm install apollo-link-ws subscriptions-transport-ws --save
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/ApolloClient.re` and update the `link` as follows:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/App.js" text="src/App.js" />

```javascript


+let connectionParams = Json.Encode.object_([("headers", headers)]);

-let link = ApolloLinks.createHttpLink(
-  ~uri="https://learn.hasura.io/graphql",
-  ~headers=headers,
-  ()
-);

+
+let link = ApolloLinks.webSocketLink(
+  ~uri="wss://learn.hasura.io/graphql",
+  ~reconnect=true,
+  ~connectionParams=connectionParams,
+  ()
+)
```

Note that we are replacing HttpLink with WebSocketLink and hence all GraphQL queries go through a single websocket connection. Look at how we are passing the headers. While initializing websocket link, we need to pass headers in the `headers` key in the `connectionParams` accepted by `ApolloLinks.webSocketLink`.