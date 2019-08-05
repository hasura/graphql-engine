# svelte-graphql-app

A sample [Svelte 3](https://svelte.dev) app to demonstrate usage of GraphQL Queries, Mutations and Subscriptions with [svelte-apollo](https://github.com/timhall/svelte-apollo), Hasura GraphQL engine and Postgres as database. Forked from the standard svelte [template](https://github.com/sveltejs/template)

[![Edit svelte-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/svelte-apollo?fontsize=14)

## Deploy Hasura

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
columns: `id`, `title`, `content`, `author_id` (foreign key to `author` table's `id`) and `created_at`

  ![Create foreign key for author_id column to author's id](./assets/author_fk.png)

- Now create a relationship from article table to author table by going to the Relationships tab.

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/svelte-apollo
  ```

## Setup App

Install the dependencies...

```bash
npm install
```

- Open `src/apollo.js` and configure Hasura's GraphQL Endpoint as follows: 

```javascript

  const wsLink = new WebSocketLink({
    uri: "ws://localhost:8080/v1/graphql",
    options: {
      reconnect: true,
      lazy: true
    },
    connectionParams: () => {
      return { headers: getHeaders() };
    },
  });

  const httpLink = new HttpLink({
    uri: "http://localhost:8080/v1/graphql",
    headers: getHeaders()
  });

```
Replace the `uri` argument with your Hasura GraphQL Endpoint for both `wsLink` and `httpLink`

Start [Rollup](https://rollupjs.org):

```bash
npm run dev
```

Navigate to [localhost:5000](http://localhost:5000). You should see your app running. Edit a component file in `src`, save it, and reload the page to see your changes.

## Deploying to the web

### With [now](https://zeit.co/now)

Install `now` if you haven't already:

```bash
npm install -g now
```

Then, from within your project folder:

```bash
now
```

This will deploy the app on Now 2.0 Platform and you have the Svetle app running live :)

