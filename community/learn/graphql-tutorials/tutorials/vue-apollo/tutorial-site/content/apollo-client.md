---
title: "Set up a GraphQL client with Apollo"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/iph-ERuYx_Y" />

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `axios` or `fetch` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

Let's get started by installing apollo client & peer graphql dependenices:

```bash
$ npm install --save vue-apollo graphql apollo-client apollo-link-http apollo-cache-inmemory graphql-tag
```

## Create Apollo Client Instance

Open `src/components/main.js` and add the following code to create an ApolloClient instance.

```javascript
  import Vue from "vue";
  import App from "./App.vue";
  import router from "./router";
  import AuthPlugin from "./plugins/auth";

+ import ApolloClient from "apollo-client";
+ import { HttpLink } from "apollo-link-http";
+ import { InMemoryCache } from "apollo-cache-inmemory";

  Vue.use(AuthPlugin);

  Vue.config.productionTip = false;
+ const getHeaders = () => {
+  const headers = {};
+   const token = window.localStorage.getItem('apollo-token');
+   if (token) {
+     headers.authorization = `Bearer ${token}`;
+   }
+   return headers;
+ };
+
+ // Create an http link:
+ const link = new HttpLink({
+   uri: 'https://learn.hasura.io/graphql',
+   fetch,
+   headers: getHeaders()
+ });
+
+ const client = new ApolloClient({
+   link: link,
+   cache: new InMemoryCache({
+     addTypename: true
+   })
+ });

  new Vue({
    router,
    render: h => h(App)
  }).$mount("#app");

```

These are the required apollo dependenices to get started. We have also written a simple utility to get token information to construct the headers.

## Install VueApollo Plugin

Now let's install the VueApollo plugin into Vue.

```javascript
  import Vue from "vue";
  import App from "./App.vue";
  import router from "./router";
  import AuthPlugin from "./plugins/auth";

+ import VueApollo from "vue-apollo";

  import ApolloClient from "apollo-client";
  import { HttpLink } from "apollo-link-http";
  import { InMemoryCache } from "apollo-cache-inmemory";

  Vue.use(AuthPlugin);
+ Vue.use(VueApollo);

```

## Add the ApolloProvider

Finally, lets add the ApolloProvider. The provider holds the Apollo client instances that can then be used by all the child components.

```javascript
+  const apolloProvider = new VueApollo({
+    defaultClient: client,
+  })

  new Vue({
    router,
+   apolloProvider,
    render: h => h(App)
  }).$mount("#app");

```
Let's try to understand what is happening here. 

We are creating an `HttpLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate ApolloClient by passing in our HttpLink and a new instance of `InMemoryCache` (recommended caching solution). Finally we are adding apollo provider to the Vue app.
