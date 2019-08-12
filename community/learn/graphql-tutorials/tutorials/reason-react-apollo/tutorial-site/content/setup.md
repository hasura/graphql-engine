---
title: "Tutorial & boilerplate setup"
metaTitle: "Todo app boilerplate setup | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "The GraphQL backend is already ready. The task is to convert the static UI into a working realtime app in ReasonReact"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be to convert the "static" UI into a working realtime app.

### Download and run the boilerplate

1. Download the boilerplate from https://learn.hasura.io/graphql/reason-react-apollo/boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. Run the following commands:
    - `npm install` to install the dependencies
    - `npm start` to compile the `.re` files to `.bs.js` files watch the source code for changes
    - In a new shell, run `npm run webpack` to bundle the `.bs.js` files
    - In another new shell, serve the build folder to serve the single page app (`npm install -g serve && serve -s -p 3000 build`)
    - Try opening your app at http://localhost:3000
4. Signup/login as a user to load the todo app page

This is what you should see after the steps above:

![Boilerplate after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/boilerplate-after-login.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphiql-after-login.png)
