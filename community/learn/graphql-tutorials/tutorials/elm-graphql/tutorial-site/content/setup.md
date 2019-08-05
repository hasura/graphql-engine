---
title: "Tutorial & boilerplate setup"
metaTitle: "Todo app elm boilerplate setup | GraphQL Elm Tutorial"
metaDescription: "The GraphQL backend is already ready. The task is to convert the static UI into a working realtime app in Elm"
---

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be to convert the "static" UI into a working realtime app.

### Download and run the boilerplate

1. Download the boilerplate from https://learn.hasura.io/graphql/elm/elm-boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. Install dependencies and run the "static" app
    - `npm install`
    - `npm start`
4. Signup/login as a user to load the todo app page

This is what you should see after the steps above:

![Boilerplate after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/boilerplate-after-login.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql?tutorial=react-native
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphiql-after-login.png)
