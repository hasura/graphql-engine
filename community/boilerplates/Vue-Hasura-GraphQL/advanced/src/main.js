// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'
import router from './router'
import ApolloClient from 'apollo-client'
import { HttpLink } from 'apollo-link-http'
import { InMemoryCache } from 'apollo-cache-inmemory'
import BootstrapVue from 'bootstrap-vue'
import VueApollo from 'vue-apollo'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'
import VueMoment from 'vue-moment'
import { split } from 'apollo-link'
import { WebSocketLink } from 'apollo-link-ws'
import { getMainDefinition } from 'apollo-utilities'
import { SubscriptionClient } from 'subscriptions-transport-ws'
import { setContext } from 'apollo-link-context'

const getHeaders = () => {
  const token = localStorage.getItem('access_token')
  const headers = {
    authorization: token ? `Bearer ${token}` : ''
  }
  return headers
}

const authLink = setContext((_, { headers }) => {
  const token = localStorage.getItem('access_token')
  return {
    headers: {
      ...headers,
      authorization: token ? `Bearer ${token}` : ''
    }
  }
})

const token = localStorage.getItem('access_token')
// Create an http link:
const httpLink = new HttpLink({
  uri: process.env.GRAPHQL_ENDPOINT,
  fetch,
  headers: getHeaders(token)
})

// Create the subscription websocket link
const wsLink = new WebSocketLink(
  new SubscriptionClient(process.env.GRAPHQL_WS_ENDPOINT, {
    reconnect: true,
    timeout: 30000,
    connectionParams: {
      headers: getHeaders(token)
    }
  })
)

// using the ability to split links, you can send data to each link
// depending on what kind of operation is being sent
const link = split(
  // split based on operation type
  ({ query }) => {
    const { kind, operation } = getMainDefinition(query)
    return kind === 'OperationDefinition' && operation === 'subscription'
  },
  wsLink,
  httpLink
)

// Create the apollo client
const client = new ApolloClient({
  link: authLink.concat(link),
  cache: new InMemoryCache({
    addTypename: true
  })
})

// install the vue plugin
Vue.use(VueApollo)
Vue.use(BootstrapVue)
Vue.use(VueMoment)

const apolloProvider = new VueApollo({
  defaultClient: client,
  defaultOptions: {
    $loadingKey: 'loading'
  },
  errorHandler (error) {
    console.log('Global error handler')
    console.error(error)
  }
})

Vue.config.productionTip = false

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  apolloProvider,
  components: { App },
  template: '<App/>'
})
