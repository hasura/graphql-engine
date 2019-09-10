---
title: "Apollo Subscription Component"
metaTitle: "Apollo Subscription Component | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "The easiest way to bring live data to your UI is using the Subscription component from Apollo."
---

The easiest way to bring live data to your UI is using the typed `Subscription` component, just like we generated `Query` and `Mutation` components in the earlier sections. This lets you render the stream of data from your service directly within your render function of your component! One thing to note, subscriptions are just listeners, they don’t request any data when first connected, but only open up a connection to get new data.