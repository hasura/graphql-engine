// in memory cache for caching GraphQL data 
let cache = ApolloInMemoryCache.createInMemoryCache();

// apollo link instance as a network interface for apollo client
// apollo link also has to be configured with headers
// we get token from local storage and configure apollo link headers with it
let headers = switch(Util.getTokenFromStorage()) {
  | None => Json.Encode.object_([])
  | Some(token) => Json.Encode.object_([("Authorization", Json.Encode.string("Bearer " ++ token))])
};

let connectionParams = Json.Encode.object_([("headers", headers)]);

let link = ApolloLinks.webSocketLink(
  ~uri="wss://learn.hasura.io/graphql",
  ~reconnect=true,
  ~connectionParams=connectionParams,
  ()
);

// apollo client instance
let instance = ReasonApollo.createApolloClient(~link, ~cache, ());

[@bs.module] external gql: ReasonApolloTypes.gql = "graphql-tag";
