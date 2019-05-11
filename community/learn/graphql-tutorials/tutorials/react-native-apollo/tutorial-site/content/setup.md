---
title: "Tutorial & boilerplate setup"
---

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be convert the "static" UI into a working realtime app.

### Download and run the boilerplate

<!-- FIXME: Add the zip URL here -->

1. Download the boilerplate at: https://learn.hasura.io/graphql/react-native/boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. Make sure you have `expo-cli` installed
    - `npm install -g expo-cli`
4. Install dependencies and run the app. This will start the development server
    - `npm install`
    - `npm start`
5. Open this app from your phone using `Expo`
6. Signup/login as a user to load the todo app screen

After you login, you should get something like this:

![UI after logging in](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react-native/ui-after-login.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphiql-after-login.png)
