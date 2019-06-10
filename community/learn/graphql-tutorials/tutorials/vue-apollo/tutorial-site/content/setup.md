---
title: "Tutorial & boilerplate setup"
metaTitle: "Todo app vue boilerplate setup | GraphQL Vue Apollo Tutorial"
metaDescription: "The GraphQL backend is already ready. The task is to convert the static UI into a working realtime app in Vue.js"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/JCOgZl-nNUg" />

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be convert the "static" UI into a working realtime app.

### Download and run the boilerplate

<!-- FIXME: Add the zip URL here -->

1. Download the boilerplate at: https://learn.hasura.io/graphql/vue/boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. Install dependencies and run the "static" app
    - `npm install`
    - `npm run serve`
4. Signup/login as a user to load the todo app page

This is what you should see after the steps above:

![Boilerplate after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-vue/boilerplate-after-login.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-vue/graphiql-after-login.png)
