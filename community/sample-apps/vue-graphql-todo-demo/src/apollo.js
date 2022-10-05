import Vue from 'vue'
import VueApollo from 'vue-apollo'
import { ApolloClient } from 'apollo-client'
import { HttpLink } from 'apollo-link-http'
import { InMemoryCache } from 'apollo-cache-inmemory'

const httpLink = new HttpLink({
     // You should use an absolute URL here
     uri: 'https://fast-stallion-23.hasura.app/v1/graphql',
     headers: {
          "x-hasura-admin-secret": "zubicpl4e8ifdzKuAYlTgZ3kr5KkTJtckZEuK45t8R1TcPHvmeql4hVyGDvQk7vO"
     }
})

// Create the apollo client
export const apolloClient = new ApolloClient({
     link: httpLink,
     cache: new InMemoryCache(),
     connectToDevTools: true
})

const apolloProvider = new VueApollo({
     defaultClient: apolloClient
})

// Install the vue plugin
Vue.use(VueApollo)

export default apolloProvider
