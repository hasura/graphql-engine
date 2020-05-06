# vuetify-vuex-todo-graphql
A simple Todo PWA (Progressive Web App) inspired by [TodoMVC](http://todomvc.com), [Vue.js](https://vuejs.org), [Vuex](https://vuex.vuejs.org) and [Vuetify](https://vuetifyjs.com) technologies, forked from [davidgararo/vuetify-todo-pwa](https://github.com/davidgaroro/vuetify-todo-pwa), adding Hasura GraphQL integration

[![Edit vuetify-vuex-todo-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/vuetify-vuex-todo-graphql?fontsize=14)

# Tutorial

- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

  Please checkout our [docs](https://hasura.io/docs/1.0/graphql/manual/deployment/index.html) for other deployment methods

- Get the Heroku app URL (say `my-app.herokuapp.com`)
- Create `todos` table:
  
  Open Hasura console: visit https://my-app.herokuapp.com on a browser  
  Navigate to `Data` section in the top nav bar and create a table as follows:

  ![Create todos table](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/assets/vuetify-vuex-todo-graphql/create_table_todos.png)

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/vuetify-vuex-todo-graphql
  ```

- Install node modules:
  ```bash
  npm install
  ```

- Open `src/apollo.js` and configure Hasura's GraphQL Endpoint as follows: 
```js

import Vue from 'vue'
import VueApollo from 'vue-apollo'
import { ApolloClient } from 'apollo-client'
import { HttpLink } from 'apollo-link-http'
import { InMemoryCache } from 'apollo-cache-inmemory'

const httpLink = new HttpLink({
  // You should use an absolute URL here
  uri: 'https://myapp.herokuapp.com/v1/graphql'
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

```

In the `httpLink`, replace `myapp.herokuapp.com` with your own Heroku URL of Hasura GraphQL Engine that you deployed above

- Compile and hot-reload for development
```bash
  npm run serve
```

- Production Build
```bash
  npm run build
```

