import { InMemoryCache } from 'apollo-cache-inmemory'
import { ApolloClient } from 'apollo-client'
import { ApolloLink, split } from 'apollo-link'
import { setContext } from 'apollo-link-context'
import { HttpLink } from 'apollo-link-http'
import { WebSocketLink } from 'apollo-link-ws'
import { getMainDefinition } from 'apollo-utilities'
import { OperationDefinitionNode } from 'graphql'
import { getAuthHeader } from './services/auth.service'

const httpUri = process.env.REACT_APP_SERVER_URL ? process.env.REACT_APP_SERVER_URL : 'https://whatsapp-clone.demo.hasura.app/v1/graphql'
const wsUri = httpUri.replace(/^https?/, process.env.REACT_APP_ENV === 'dev' ? 'ws' : 'wss')

const httpLink = new HttpLink({
  uri: httpUri,
})

const wsLink = new WebSocketLink({
  uri: wsUri,
  options: {
    lazy: true,
    reconnect: true,
    connectionParams: () => {
      return { headers: {'Authorization': getAuthHeader()} };
    },
  },
})

const authLink = setContext((_, { headers }) => {
  const auth = getAuthHeader()

  return {
    headers: {
      ...headers,
      Authorization: auth,
    },
  }
})

const terminatingLink = split(
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query) as OperationDefinitionNode
    return kind === 'OperationDefinition' && operation === 'subscription'
  },
  wsLink,
  authLink.concat(httpLink),
)

const link = ApolloLink.from([terminatingLink])
const cache = new InMemoryCache()

export default new ApolloClient({
  link,
  cache,
})
