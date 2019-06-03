---
title: "Tutorial & boilerplate setup"
metaTitle: "Todo app Android boilerplate setup | GraphQL Android Apollo Tutorial"
metaDescription: "The GraphQL backend is already ready. The task is to convert the static UI into a working realtime app in Android"
---

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be to convert the "static" UI into a working realtime app.

### Download and run the boilerplate

1. Download the boilerplate at: https://learn.hasura.io/graphql/android/boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. This project uses gradle for dependency resolution, so you would need latest version of gradle installed.
4. Sync Project with Gradle files
5. Run the app in android studio and signup with Auth0 to view the app

This is what you should see after the steps above:

![Boilerplate login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-android/boilerplate-login.png)
![Boilerplate after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-android/boilerplate-private-todo.png)
![Boilerplate feed](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-android/boilerplate-public-feed.png)
![Boilerplate online](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-android/boilerplate-online-users.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphiql-after-login.png)
