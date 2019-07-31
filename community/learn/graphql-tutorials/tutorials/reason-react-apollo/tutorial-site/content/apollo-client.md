---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client GraphQL Setup | GraphQL ReasonML React Apollo Tutorial"
metaDescription: "You will learn how to configure Apollo Client in ReasonReact by installing dependencies like react-apollo, apollo-client, apollo-link-http, apollo-cache-inmemory"
---

import GithubLink from "../src/GithubLink.js";
import YoutubeEmbed from "../src/YoutubeEmbed.js";

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `Fetch.fetch`. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

## Installation

Let's get started by installing apollo client & peer graphql dependencies:

```bash
$ npm install --save apollo-client react-apollo reason-apollo apollo-cache-inmemory apollo-link-http graphql graphql-tag
```

You also need to add a helper package called `graphql_ppx` to the dev dependencies to handle GraphQL query and mutation types elegantly.

```bash
npm install --save-dev graphql_ppx
```

Also, add `reason-apollo` and `graphql_ppx` to the `bs-dependencies` and `ppx_flags` respectively in your `bsconfig.json`.

```json
{
  "bs-dependencies": [
    ...,
    "reason-apollo"
  ],
  "ppx-flags": [
    "graphql_ppx/ppx"
  ]  
}
```

Finally, you need a `graphql_schema.json` in the root of your project so that the GraphQL queries and mutations are type checked against it. To get `graphql_schema.json`,

1. Go to https://learn.hasura.io/graphql/graphiql and login
2. Copy the JWT from headers:
  ![graphiql-jwt-copy](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-reason-react-apollo/graphiql-jwt-copy.png)
3. Run this command from the root of your project:

  ```js
  npx send-introspection-query https://learn.hasura.io/graphql --headers "Authorization: Bearer <JWT>"
  ```

## Setup

Create a file called `src/ApolloClient.re` and create an instance of Apollo Client in it as follows:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/ApolloClient.re" text="ApolloClient.re" />

```js
// in memory cache for caching GraphQL data 
let cache = ApolloInMemoryCache.createInMemoryCache();

// apollo link instance as a network interface for apollo client
// apollo link also has to be configured with headers
// we get token from local storage and configure apollo link headers with it
let headers = switch(Util.getTokenFromStorage()) {
  | None => Json.Encode.object_([])
  | Some(token) => {
    Js.log(token);
    Json.Encode.object_([("Authorization", Json.Encode.string("Bearer " ++ token))])
  }
};

let link = ApolloLinks.createHttpLink(
  ~uri="https://learn.hasura.io/graphql",
  ~headers=headers,
  ()
);

// apollo client instance
let instance = ReasonApollo.createApolloClient(~link, ~cache, ());

[@bs.module] external gql: ReasonApolloTypes.gql = "graphql-tag";
```


Let's try to understand what is happening here. 

We are creating an `HttpLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql). We are also configuring the headers with the JWT token from the local storage.

At the end, we instantiate ApolloClient by passing in our `HttpLink` and a new instance of `InMemoryCache` (recommended caching solution). This instance can be used anywhere in the application as `.

We also write a custom binding for [`gql`](https://github.com/apollographql/graphql-tag) so that we can fire GraphQL Queries manually without the JSX components. (more on this later)

Finally, we have to use this instance in `src/App.re` to provide the child application with Apollo Client so that its features can be leveraged throughout the application.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/reason-react-apollo/app-final/src/App.re" text="App.re" />

```js
[@react.component]
let make = () => {
-  <div>
+  <ReasonApollo.Provider client=ApolloClient.instance>
    <Navbar />
    <div className="container-fluid p-left-right-0">
      <div className="col-xs-12 col-md-9 p-left-right-0">
        <div className="col-xs-12 col-md-6 sliderMenu p-30">
          <TodoPrivateWrapper />
        </div>
        <div className="col-xs-12 col-md-6 sliderMenu p-30 bg-gray border-right">
          <TodoPublicWrapper />
        </div>
      </div>
      <div className="col-xs-12 col-md-3 p-left-right-0">
        <div className="col-xs-12 col-md-12 sliderMenu p-30 bg-gray">
          <OnlineUsersWrapper />
        </div>
      </div>
    </div>
-  </div>
+  </ReasonApollo.Provider>
}
```
