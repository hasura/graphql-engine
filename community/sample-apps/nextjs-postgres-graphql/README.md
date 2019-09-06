# nextjs-postgres-graphql

Boilerplate to get started with Nextjs, Hasura GraphQL engine as CMS and postgres as database using this awesome library: [withData](https://github.com/adamsoffer/next-apollo).

[![Edit nextjs-postgres-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/nextjs-postgres-graphql?fontsize=14)

![Nextjs Postgres GraphQL](./assets/nextjs-postgres-graphql.png)

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

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/nextjs-postgres-graphql
  ```

- Install npm modules:
  ```bash
  npm install
  ```

- Create config.js as follows, in this step we are configuring `withData` with an `httpLink` to connect to a valid GraphQL server URL.
  ```js
  import { withData } from 'next-apollo'
  import { HttpLink } from 'apollo-link-http'
  
  // can also be a function that accepts a `context` object (SSR only) and returns a config
  const config = {
    link: new HttpLink({
      uri: 'https://my-app.herokuapp.com/v1/graphql', // <- Configure GraphQL Server URL (must be absolute)
    })
  }

  export default withData(config)
  ```

- Wrap your page component with `Query` component from `react-apollo` so that appropriate data can be fetched while the page is SSRed
    - GraphQL query

      ```js

      const query = gql`
      	query {
      	  author {
      	    id
      	    name
      	  }
      	}
      `

      ```
    - Wrap your component with `Query`
      ```js

        <Query    // <- Wrap the component which requires data with Query component from react-apollo
          query={ query }
          fetchPolicy={ 'cache-and-network' }
        >
          {({ loading, data: { author:authors }}) => {
            return (
              <div>
                <AuthorList authors={authors} />
              </div>
            );
          }}
        </Query>

      ```


- Run the app:
  ```bash
  npm run dev -- -p 8000
  ```
- Test the app
  Visit [http://localhost:8000](http://localhost:8000) to view the app

  ![Demo app](../gatsby-postgres-graphql/assets/test_app.jpg)

# How it works

  It uses [next-apollo](https://github.com/adamsoffer/next-apollo#how-does-it-work) underneath which ensures that data requirement is satisfied before it is rendered on the server and next.js takes care of the rest.

# Contributing

Checkout the [contributing guide](../../../CONTRIBUTING.md#community-content) for more details.
