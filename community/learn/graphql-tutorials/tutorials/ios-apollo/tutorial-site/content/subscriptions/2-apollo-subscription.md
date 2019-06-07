---
title: "Apollo Subscriptions"
metaTitle: "Apollo Subscription in iOS | GraphQL iOS Apollo Tutorial"
metaDescription: "The easiest way to bring live data to your UI is using the apollo.subscribe#subscription from apollo-ios."
---

The easiest way to bring live data to your UI is using the Subscription from `apollo.subscribe#subscription`. This lets you render the stream of data from your service directly within your UI! One thing to note, subscriptions are just listeners, they don’t request any data when first connected, but only open up a connection to get new data.