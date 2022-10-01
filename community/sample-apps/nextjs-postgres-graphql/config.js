import withApollo from "next-with-apollo";
import { ApolloClient, ApolloProvider, InMemoryCache } from "@apollo/client";

// Learn more about `@apollo/client` here - https://www.apollographql.com/docs/react/why-apollo
// Learn more about `next-with-apollo` here - https://github.com/lfades/next-with-apollo

// creating the Apollo Client
const client = new ApolloClient({
  headers: {
    "x-hasura-admin-secret": process.env.NEXT_PUBLIC_HASURA_ADMIN_SECRET
  },
  uri: process.env.NEXT_PUBLIC_HASURA_APP_URL, // <- Configure GraphQL Server URL (must be absolute)
  cache: new InMemoryCache()
});

export default withApollo(
  () => {
    return client;
  },
  {
    // providing the Apollo Client access to the pages
    render: ({ Page, props }) => {
      return (
        <ApolloProvider client={props.apollo}>
          <Page {...props} />
        </ApolloProvider>
      );
    }
  }
);
