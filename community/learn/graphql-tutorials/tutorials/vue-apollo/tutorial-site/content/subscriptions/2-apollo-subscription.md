---
title: "Apollo Smart Subscription"
metaTitle: "Vue Apollo Smart Subscription | GraphQL Vue Apollo Tutorial"
metaDescription: "The easiest way to bring live data to your Vue app UI is using the Smart Subscription from vue-apollo, apollo.$subscribe"
---

The easiest way to bring live data to your UI is using the Smart Subscription from vue-apollo. Each subscription declared in the apollo.$subscribe option in a component results in the creation of a smart subscription object.

One thing to note, subscriptions are just listeners, they don’t request any data when first connected, but only open up a connection to get new data.