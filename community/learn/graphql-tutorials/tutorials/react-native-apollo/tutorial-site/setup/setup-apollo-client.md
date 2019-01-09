# Apollo setup

## Install GraphQL modules

```
npm install apollo-boost react-apollo graphql apollo-link-ws apollo-utilities subscriptions-transport-ws --save
```

The above command will install the following:

- [`apollo-boost`](https://github.com/apollographql/apollo-client/tree/master/packages/apollo-boost): Package containing basic Apollo client libraries
- [`react-apollo`](https://github.com/apollographql/react-apollo): View layer integration for React
- [`graphql`](https://github.com/graphql/graphql-js): Standard GraphQL library for Javascript
- [`apollo-link-ws`](https://github.com/apollographql/apollo-link/tree/master/packages/apollo-link-ws): WebSocket network layer from Apollo client for realtime support
- [`subscriptions-transport-ws`](https://github.com/apollographql/apollo-link/tree/master/packages/apollo-link-ws): A WebSocket client for GraphQL subscriptions for realtime support
- [`apollo-utilities`](https://github.com/apollographql/apollo-client/tree/master/packages/apollo-utilities): Utility library for parsing GraphQL queries


## Instantiating Apollo Client

As mentioned before, we will use Apollo client for GraphQL in this app. To instantiate Apollo client, we need to provide an instance of [Apollo link](https://www.apollographql.com/docs/link/index.html) that has information about GraphQL API and the protocol to be used while querying.

> Apollo link is a network layer that Apollo client uses to connect to the remote GraphQL API. We can customize how to contact the GraphQL API by adding custom middleware, custom HTTP/WebSocket parameters etc.

### Creating Apollo Link

#### Introduction

This todo app will be a realtime application. We will be using GraphQL subscriptions over WebSocket. So we need to create an instance of Apollo Link such that it uses HTTP for queries and mutations but uses WebSockets for Subscriptions. To achieve this, the ideal way is:

1. Create an `[apollo-link-http](https://www.apollographql.com/docs/link/links/http.html)` (let us call this httpLink) which is an extension of `apollo-link` that makes queries and mutations over HTTP. 
2. Create an `[apollo-link-ws](https://www.apollographql.com/docs/link/links/ws.html)`(let us call this wsLink) which is an extension of `apollo-link` that makes subscriptions over WebSocket.
3. Create a new instance of `apollo-link` using the `split` function from the `apollo-link` package so that we can use `wsLink` for subscriptions and `httpLink` for queries and mutations.

#### Authentication

This app uses JWT based authentication. We have discussed about that in detail in  the next section. For now, it is enough to understand that JWT is a token that we need to send with every request and the server authenticates the requests using this JWT.

Since this token must be sent with all requests, it is best to set this token in the Apollo link itself. 

#### HTTP Link

An HTTP Link is created as follows:

```js
import { HttpLink } from 'apollo-link-http';

const httpLink = new HttpLink({
  uri: `https://${GRAPHQL_API}/graphql`,
  headers: {
    'Authorization': 'Bearer <token>'
  }
});
```

#### Websocket Link

Creating a websocket link is as simple as given below. Please note that we need to set headers in the `connectionParams > headers` field.

```js
import { WebSocketLink } from 'apollo-link-ws';

const wsLink = new WebSocketLink({
  uri: `wss://${GRAPHQL_API}/graphql``,
  options: {
    reconnect: true,
    connectionParams: {
      headers: {
        Authorization: `Bearer ${token}`
      }
    }
  }
});
```

#### Conditional Link

Now that we have both WS and HTTP links, we need to create a new instance of Apollo Link that uses `wsLink` for subscriptions and `httpLink` otherwise. To do that, we can use the `split` function from the `apollo-link` library.

```js
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";

const conditionalLink = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === 'OperationDefinition' && operation === 'subscription';
  },
  wsLink,
  httpLink,
);
```

This link can be supplied to `ApolloClient` to instantiate it.

#### Instantiating the client

We can instantiate Apollo client by providing the link:

```js
import { ApolloClient } from 'apollo-client';

const client = new ApolloClient({
  link: conditionalLink
});
```

#### With token

As you must have observed above, we need the JWT While creating the client, which means that we can instantiate the client only after the authentication step has been completed. So let us expose a function that takes a token, creates ApolloClient with the token and returns it.

Create a file called `apollo.js` at the root level and add the following code to it.

```js
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from "apollo-cache-inmemory";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";
import { AsyncStorage } from 'react-native';

const GRAPHQL_ENGINE_ENDPOINT = 'hasura-todo-test.herokuapp.com/v1alpha1/graphql'

const makeApolloClient = (token) => { 
  const httpLink = new HttpLink({
    uri: `https://${GRAPHQL_ENGINE_ENDPOINT}`,
    headers: {
      Authorization: `Bearer ${token}`
    }
  });
  const wsLink = new WebSocketLink({
    uri: `wss://${GRAPHQL_ENGINE_ENDPOINT}`,
    options: {
      reconnect: true,
      connectionParams: {
        headers: {
          Authorization: `Bearer ${token}`
        }
      }
    }
  });

  const link = split(
    // split based on operation type
    ({ query }) => {
      const { kind, operation } = getMainDefinition(query);
      return kind === 'OperationDefinition' && operation === 'subscription';
    },
    wsLink,
    httpLink,
  );
  const client = new ApolloClient({
    link,
    cache: new InMemoryCache()
  });

  return client;
}

export default makeApolloClient;
```

This function can now be used whenever and wherever we need to instantiate the ApolloClient.
