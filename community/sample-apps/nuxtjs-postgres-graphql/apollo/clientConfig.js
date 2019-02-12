import { InMemoryCache } from "apollo-cache-inmemory";
export default function(context){
  return {
    httpLinkOptions: {
      // uri: 'https://my-app.herokuapp.com/v1alpha1/graphql',
      uri: 'http://localhost:8080/v1alpha1/graphql',
      // headers: {
      //   'x-hasura-access-key': '<YOUR_SECRET_KEY>'
      // },
      credentials: 'same-origin'
    },
    cache: new InMemoryCache(),
    // wsEndpoint: 'wss://my-app.herokuapp.com/v1alpha1/graphql',
    wsEndpoint: 'ws://localhost:8080/v1alpha1/graphql',
  }
}
