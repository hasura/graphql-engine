---
title: "Tutorial & boilerplate setup"
metaTitle: "Todo app iOS boilerplate setup | GraphQL iOS Apollo Tutorial"
metaDescription: "The GraphQL backend is already ready. The task is to convert the static UI into a working realtime app in iOS"
---

For this tutorial, the GraphQL backend and the basic app UI is already ready.
Our task will be to convert the "static" UI into a working realtime app.

### Download and run the boilerplate

1. Download the boilerplate at: https://learn.hasura.io/graphql/ios/boilerplate.zip
2. Unzip and make sure you're in the `app-boilerplate` directory
3. This project uses carthage for dependency resolution, so you would need carthage installed,

```bash
$ brew install carthage
```

or you can use pacakge installation from [Carthage Latest Release](https://github.com/Carthage/Carthage/releases)

4. Install dependencies with Carthage.

```bash
$ carthage update --platform iOS
```

6. Run the app in xcode and signup with auth0 to view the app

This is what you should see after the steps above:

![Boilerplate login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-ios/boilerplate-login.png )
![Boilerplate after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-ios/boilerplate-todos-landing-selected.png)
![Boilerplate feed](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-ios/boilerplate-feed.png)
![Boilerplate online](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-ios/boilerplate-online.png)

### Load GraphiQL to play with your GraphQL APIs

1. Head to https://learn.hasura.io/graphql/graphiql
2. Log in (so that you can test the GraphQL APIs with a valid user token)

This is what you should see after the steps above:

![GraphiQL after login](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-react/graphiql-after-login.png)
