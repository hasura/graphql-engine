---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client with Hooks GraphQL Setup | GraphQL React Apollo Typescript Tutorial"
metaDescription: "You will learn how to configure Apollo Client in React by installing dependencies like @apollo/react-hooks, apollo-client, apollo-link-http, apollo-cache-inmemory"
---

import GithubLink from "../src/GithubLink.js";

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `axios` or `fetch` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

### Apollo Client & Apollo React Hooks Installation
Let's get started by installing apollo client & peer graphql dependencies:

```bash
$ yarn add apollo-client @apollo/react-hooks apollo-cache-inmemory apollo-link-http graphql graphql-tag
```

### Create Apollo Client Instance
Open `src/components/App.tsx` and add the following imports at the top:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/typescript-react-apollo/app-final/src/components/App.tsx" text="src/components/App.tsx" />

```javascript
import * as React from 'react';

+ import ApolloClient from 'apollo-client';
+ import { InMemoryCache } from 'apollo-cache-inmemory';
+ import { HttpLink } from 'apollo-link-http';
+ import { ApolloProvider } from '@apollo/react-hooks';

import Header from './Header';
import TodoPrivateWrapper from './Todo/TodoPrivateWrapper';
import TodoPublicWrapper from './Todo/TodoPublicWrapper';
import OnlineUsersWrapper from './OnlineUsers/OnlineUsersWrapper';

import { useAuth0 } from "./Auth/react-auth0-spa";

const App = ({idToken}:{idToken:string}) => {
  const { loading, logout  } = useAuth0();
  if(loading) {
    return (<div>Loading...</div>);
  }
  return (
    <div>
      <Header logoutHandler={logout} />
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
    </div>
  );
};

export default App;
```

These are the required apollo dependencies to get started. Now let's define a function which will return apollo client with httplink and cache.

```javascript
import { useAuth0 } from "./Auth/react-auth0-spa";

+ const createApolloClient = (authToken: string) => {
+  return new ApolloClient({
+    link: new HttpLink({
+      uri: 'https://learn.hasura.io/graphql',
+      headers: {
+        Authorization: `Bearer ${authToken}`
+      }
+    }),
+    cache: new InMemoryCache(),
+  });
+ };
```
Create the apollo client inside `App` and pass the client prop to `<ApolloProvider>` component.

```javascript
const App = ({idToken}:{idToken:string}) => {
  const { loading, logout  } = useAuth0();
  if(loading) {
    return (<div>Loading...</div>);
  }
+ const client = createApolloClient(idToken);
  return (
+   <ApolloProvider client={client}>
      <div>
      </div>
+   </ApolloProvider>
  );
};
```

Let's try to understand what is happening here. 

### HttpLink and InMemoryCache
We are creating an `HttpLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate ApolloClient by passing in our HttpLink and a new instance of `InMemoryCache` (recommended caching solution). We are wrapping all of this in a function which will return the client.

We are going to make use of this function inside `App` component.
