---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client GraphQL Setup | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn how to configure Apollo Client in React Native by installing dependencies like react-apollo, apollo-client, apollo-link-http, apollo-cache-inmemory"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";
import GithubLink from "../src/GithubLink.js";

<YoutubeEmbed link="https://www.youtube.com/embed/XSR6jH5doXk" />

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `axios` or `fetch` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

### React Native Apollo Installation
Let's get started by installing apollo client & peer graphql dependencies:

```bash
$ npm install --save apollo-client react-apollo apollo-cache-inmemory apollo-link-http graphql graphql-tag
```

### Create Apollo Client Instance
Create a file called `apollo.js` inside `src` and that exports a function called `makeApolloClient` that takes the authorization token as an argument and creates an instance of apollo client with our GraphQL endpoint.

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/react-native-apollo/app-final/src/apollo.js" text="apollo.js"/>

```javascript

import { HttpLink } from 'apollo-link-http';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from "apollo-cache-inmemory";

const makeApolloClient = (token) => {

  // create an apollo link instance, a network interface for apollo client
  const link = new HttpLink({
    uri: `https://learn.hasura.io/graphql`,
    headers: {
      Authorization: `Bearer ${token}`
    }
  });

  // create an inmemory cache instance for caching graphql data
  const cache = new InMemoryCache()

  // instantiate apollo client with apollo link instance and cache instance
  const client = new ApolloClient({
    link,
    cache
  });

  return client;
}

export default makeApolloClient;
```

Let's try to understand what is happening here. 

### HttpLink and InMemoryCache
We are creating an `HttpLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate ApolloClient by passing in our HttpLink and a new instance of `InMemoryCache` (recommended caching solution). We are wrapping all of this in a function which will return the client.

We are going to make use of this function inside our entrypoint component (`src/navigation/Main.js`).

Go to `src/navigation/Main.js`, and create an instance of Apollo client using the function that we wrote above. This instance of `ApolloClient` must be passed to `ApolloProvider` that wraps the entire application. ApolloProvider provides this apollo client's to the context of the all the child components so that it can be used wherever required.

In this component, we fetch the auth token from `AsyncStorage`, create an apollo client with this token in the state. All this is performed in `componentDidMount` of the component.


```js
import React from 'react';
import Drawer from './DrawerNavigator';
+ import { AsyncStorage } from 'react-native';
+ import { ApolloProvider} from 'react-apollo';
+ import CenterSpinner from '../screens/components/CenterSpinner';
+ import makeApolloClient from '../apollo';

export default class App extends React.Component {

+ state = {
+   client: null
+ }

+ async componentDidMount() {
+   // fetch session
+   const session = await AsyncStorage.getItem('@todo-graphql:session');
+   const sessionObj = JSON.parse(session);
+   const { token, id } = sessionObj;
+   // make apollo client with this session token
+   const client = makeApolloClient(token);
+   // start emitting events saying that the useri s online
+   this.setState({ client });
+ }

  render() {

+    if (!this.state.client) {
+      return <CenterSpinner />
+    }

-   return <Drawer/>
+   return (
+      <ApolloProvider client={this.state.client}>
+        <Drawer />
+     </ApolloProvider> 
+   )
  }
};

```

Once this has been completed, we can now use the features of Apollo Client anywhere in the child application.
