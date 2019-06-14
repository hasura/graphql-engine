# nuxtjs-postgres-graphql

> Boilerplate to get started with Nuxt.js, Hasura GraphQL engine as CMS and postgres as database using the [create-nuxt-app](https://nuxtjs.org/guide/installation) and [@nuxtjs/apollo](https://github.com/nuxt-community/apollo-module) module.

[![Edit nuxtjs-postgres-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/nuxtjs-postgres-graphql?fontsize=14)

# Tutorial

- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

  Please checkout our [docs](https://docs.hasura.io/1.0/graphql/manual/deployment/index.html) for other deployment methods

- Get the Heroku app URL (say `my-app.herokuapp.com`)
- Create `author` table:
  
  Open Hasura console: visit https://my-app.herokuapp.com on a browser  
  Navigate to `Data` section in the top nav bar and create a table as follows:

  ![Create author table](../gatsby-postgres-graphql/assets/add_table.jpg)

- Insert sample data into `author` table:

  ![Insert data into author table](../gatsby-postgres-graphql/assets/insert_data.jpg)

  Verify if the row is inserted successfully

  ![Insert data into author table](../gatsby-postgres-graphql/assets/browse_rows.jpg)

- Similarly, create an article table with the following data model:
table: `article`
columns: `id`, `title`, `content`, `author_id` (foreign key to `author` table's `id`)

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/nuxtjs-postgres-graphql
  ```

- Install npm modules:
  ```bash
  npm install
  ```

- Open `apollo/clientConfig.js` and configure Hasura's GraphQL Endpoint as follows: 
  ```js

    import { InMemoryCache } from "apollo-cache-inmemory";
    export default function(context){
      return {
            httpLinkOptions: {
                uri: 'https://my-app.herokuapp.com/v1/graphql',
                credentials: 'same-origin'
            },
            cache: new InMemoryCache(),
            wsEndpoint: 'ws://my-app.herokuapp.com/v1/graphql',
      }
    }
  ```

- We have defined the graphql query in `apollo/queries/fetchAuthor.gql`. 
    - GraphQL query

    ```graphql

    query {
      author {
        id
        name
      }
    }

    ```

    - In `pages/index.vue`, we import author query.
    ```js

    <script>
    import author from '~/apollo/queries/fetchAuthor'

    export default {
      apollo: {
        author: {
          prefetch: true,
          query: author
        }
      },
      head: {
        title: 'Authors of Blog'
      }
    }
    </script>

    ```


- Run the app:
  ```bash
  npm run dev
  ```
- Test the app
  Visit [http://localhost:3000](http://localhost:3000) to view the app

For detailed explanation on how things work, checkout [Nuxt.js docs](https://nuxtjs.org).
