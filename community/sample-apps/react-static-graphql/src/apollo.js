import { ApolloClient } from 'apollo-client'
import { HttpLink } from 'apollo-link-http'
import { InMemoryCache } from 'apollo-cache-inmemory'
import fetch from 'node-fetch'

const client = new ApolloClient({
  link: new HttpLink({
    uri: 'http://localhost:8080/v1/graphql', // replace this with your own Hasura GraphQL Endpoint
    fetch
  }),
  cache: new InMemoryCache(),
})

export default client