---
title: "Apollo Subscriptions"
---

The easiest way to bring live data to your UI is using the Subscription from `apollo.subscribe#subscrition`. This lets you render the stream of data from your service directly within your UI! One thing to note, subscriptions are just listeners, they don’t request any data when first connected, but only open up a connection to get new data.