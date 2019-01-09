When we had initially set up Apollo, we used Apollo Boost to install the required dependenices. But subscriptions is an advanced use case which Apollo Boost does not support. So we have to install more dependenices to set up subscriptions.

```
$ npm install apollo-link-ws subscriptions-transport-ws --save
```

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `src/apollo.js` and add the following imports right below the other imports:

```
import { WebSocketLink } from "apollo-link-ws";
import { SubscriptionClient } from "subscriptions-transport-ws";
import { split } from "apollo-link";
import { getMainDefinition } from "apollo-utilities";
```

Update the imports from constants to also include `REALTIME_GRAPHQL_URL`.

```
import { GRAPHQL_URL, REALTIME_GRAPHQL_URL } from "./utils/constants";
```

Let's create the new WebSocket link to connect ApolloClient with our GraphQL Subscription server. Just below the `httpLink` code, place the following code snippet.

```

// Create a WebSocket link:
const wsLink = new WebSocketLink(
  new SubscriptionClient(REALTIME_GRAPHQL_URL, {
    reconnect: true,
    timeout: 30000,
    connectionParams: {
      headers: getHeaders(token)
    }
  })
);

// chose the link to use based on operation
const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query);
    return kind === "OperationDefinition" && operation === "subscription";
  },
  wsLink,
  httpLink
);
```

Here we are using `split` for proper routing of the requests; one for normal http queries and one for subscription queries.

Finally update the constructor call of ApolloClient to include the new WebSocketLink:

```
const client = new ApolloClient({
  link: authLink.concat(link), // notice the change here
  cache: new InMemoryCache({
    addTypename: true
  })
});
```

We are using the updated link to include httpLink, wsLink and the authLink.

