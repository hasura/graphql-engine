Set up Apollo Client
--------------------

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options. You can directly write queries and mutations in GraphQL and send them using an ApolloClient instance.

Let's get started by installing apollo dependenices:

```
$ npm install apollo-boost --save
```

`apollo-boost` comes bundled with several most used packages that you need to with Apollo Client. 

Here they are:
> 
> `apollo-client`: the client that can be used with any javascript front-end.
> 
> `apollo-cache-inmemory`: recommended cache implementation for Apollo Client 2.0.
> 
> `apollo-link-http`: An Apollo Link for modifying control flow of GraphQL requests.  
> 
> `apollo-link-error`: An Apollo Link for handling and inspecting errors.
> 
> `apollo-link-state`: An Apollo Link for managing local data.
> 
> `graphql-tag`: Utitily for parsing GraphQL queries. 
> 

**Note**: apollo-boost is a minimal config way to start using Apollo Client. It includes some sensible defaults.

Apart from `apollo-boost`, you also need to install `react-apollo` , `graphql` and `apollo-link-context` packages.

```
$ npm install react-apollo graphql apollo-link-context --save
```

`react-apollo` has the necessary bindings to use Apollo Client with React.

Open `src/apollo.js` and add the following imports at the top:

```

import ApolloClient from "apollo-client";
import { HttpLink } from "apollo-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";
import { setContext } from "apollo-link-context";
import { GRAPHQL_URL } from "./utils/constants";

const getHeaders = () => {
    const token = localStorage.getItem("auth0:id_token");
  const headers = {
    authorization: token ? `Bearer ${token}` : ""
  };
  return headers;
};
```

These are the required apollo dependenices to get started. We are also importing graphql endpoint constant and a header utility from our boilerplate code.

Now, add the following code below your imports to create the necessary Apollo Links and instantiate the ApolloClient.

```
const makeApolloClient = () => {
  const authLink = setContext((_, { headers }) => {
    const token = localStorage.getItem("auth0:id_token");
    return {
      headers: {
        ...headers,
        authorization: token ? `Bearer ${token}` : ""
      }
    };
  });

  const token = localStorage.getItem("auth0:id_token");
  // Create an http link:
  const httpLink = new HttpLink({
    uri: GRAPHQL_URL,
    fetch,
    headers: getHeaders(token)
  });

  const client = new ApolloClient({
    link: authLink.concat(httpLink),
    cache: new InMemoryCache({
      addTypename: true
    })
  });

  return client;
};

export default makeApolloClient;
```

Let's try to understand what is happening here. 

You are creating an authLink to set headers using `setContext` method. This will be used to pass your Auth0 `id_token` to your client.

Next, we are creating an `HttpLink` to connect ApolloClient with your GraphQL server. As you know already, our GraphQL server is running at <link>

At the end, we instantiate ApolloClient by passing in our authLink and HttpLink and a new instance of `InMemoryCache` (recommended caching solution). We are wrapping all of this in a function which will return the client.

We are going to make use of this function, which returns an instance of ApolloClient configured in our routes.

Open `src/routes.js` and add the following to finish the apollo set up:

```
import { ApolloProvider } from "react-apollo";
import makeApolloClient from "../apollo/apollo";

const client = makeApolloClient();

const provideClient = component => {
  return <ApolloProvider client={client}>{component}</ApolloProvider>;
};
```

Here we are wrapping our route with `<ApolloProvider>` passing the `client` as the prop. Update the routes of `/` and `/home` with the following code:

```
<Route
  path="/"
  render={props =>
    provideClient(<App auth={auth} client={client} {...props} />)
  }
/>
<Route
  path="/home"
  render={props =>
    provideClient(<Home auth={auth} client={client} {...props} />)
  }
/>
```

The only new change is that, we are wrapping our route component with provideClient.

Now, let's ensure that this client is passed on to our child components.

Open `src/components/Home/Home.js` and in your `return`, pass client to `<TodoPrivateWrapper>` and `<TodoPublicWrapper>`

```
...
<TodoPrivateWrapper client={this.props.client} />
...
<TodoPublicWrapper client={this.props.client} />
```

Now, both the react components have access to the client prop.





