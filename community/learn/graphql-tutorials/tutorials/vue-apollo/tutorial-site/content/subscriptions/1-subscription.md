---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL Vue Apollo Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using Vue Apollo Client by installing dependencies like apollo-link-ws, subscriptions-transport-ws. This will also have authorization token setup"
---

import GithubLink from "../../src/GithubLink.js";
import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/ZujdsxSRt48" />

When we had initially set up Apollo, we used Apollo Boost to install the required dependencies. But subscriptions is an advanced use case which Apollo Boost does not support. So we have to install more dependencies to set up subscriptions.

```bash
 npm install apollo-link-ws subscriptions-transport-ws --save
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/main.js` and update the following imports:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/vue-apollo/app-final/src/main.js" text="src/main.js" />

```javascript
- import { HttpLink } from 'apollo-link-http';
+ import { WebSocketLink } from 'apollo-link-ws';
```

Update the link by removing HttpLink and adding a WebSocketLink function to integrate WebSocketLink.

```javascript
    import Vue from "vue";
    import App from "./App.vue";
    import router from "./router";
    import AuthPlugin from "./plugins/auth";

    import VueApollo from 'vue-apollo'

    import ApolloClient from "apollo-client";
    import { WebSocketLink } from 'apollo-link-ws';
    import { InMemoryCache } from "apollo-cache-inmemory";

    Vue.use(AuthPlugin);
    Vue.use(VueApollo);

    Vue.config.productionTip = false;

    const getHeaders = () => {
      const headers = {};
      const token = window.localStorage.getItem('apollo-token');
      if (token) {
        headers.authorization = `Bearer ${token}`;
      }
      return headers;
    };

-   // Create an http link:
-   const link = new HttpLink({
-     uri: 'https://learn.hasura.io/graphql',
-     fetch,
-     headers: getHeaders()
-   });

+   // Create a WebSocket link:
+   const link = new WebSocketLink({
+     uri: 'wss://learn.hasura.io/graphql',
+     options: {
+       reconnect: true,
+       timeout: 30000,
+       connectionParams: () => {
+         return { headers: getHeaders() };
+       },
+     }
+   });

    const client = new ApolloClient({
      link: link,
      cache: new InMemoryCache({
        addTypename: true
      })
    });

    const apolloProvider = new VueApollo({
      defaultClient: client,
    })

    new Vue({
      router,
      apolloProvider,
      render: h => h(App)
    }).$mount("#app");

```

Note that we are replacing HttpLink with WebSocketLink and hence all GraphQL queries go through a single websocket connection.
