---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL Android Apollo Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using Android Apollo Client by installing dependencies like subscriptionTransportFactory. This will also have authorization token setup"
---

import GithubLink from '../../src/GithubLink.js'

When we had initially set up Apollo, we used just the network transport. But subscriptions is an advanced use case which needs more configuration. So we have to use ApolloWebSocket.

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `Network.kt` and update the following:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/android-apollo/app-final/app/src/main/java/com/hasura/todo/Todo/network/Network.kt" text="Network.kt" />

```kotlin

+ private val GRAPHQL_WEBSOCKET_ENDPOINT: String = "wss://learn.hasura.io/graphql"

apolloClient = ApolloClient.builder()
            .serverUrl(GRAPHQL_ENDPOINT)
            .okHttpClient(okHttpClient)
            .normalizedCache(normalizedCacheFactory, cacheKeyResolver)
+            .subscriptionTransportFactory(WebSocketSubscriptionTransport.Factory(GRAPHQL_WEBSOCKET_ENDPOINT, okHttpClient))
            .addCustomTypeAdapter(CustomType.TIMESTAMPTZ, dateCustomTypeAdapter)
            .build()
```

Note that we have added a new `subscriptionTransportFactory` with `GRAPHQL_WEBSOCKET_ENDPOINT` and hence all GraphQL subscription queries can go through a websocket connection.
