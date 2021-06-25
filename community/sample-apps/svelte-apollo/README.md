# svelte-graphql-app

A sample [Svelte 3](https://svelte.dev) app to demonstrate usage of GraphQL Queries, Mutations and Subscriptions with [svelte-apollo](https://github.com/timhall/svelte-apollo), Hasura Cloud and Postgres as database. Forked from the standard svelte [template](https://github.com/sveltejs/template)

[![Edit svelte-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/svelte-apollo?fontsize=14)

## Create new Hasura Cloud project

- Deploy GraphQL Engine on Hasura Cloud and setup PostgreSQL via Heroku:
  
  [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

- Get the app URL (something like `https://<my-project-name>.hasura.app`)
- Create `author` table:

  Open your Hasura Cloud project's console: visit `https://<my-project-name>.hasura.app` on a browser  
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
Replace the `uri` argument with your Hasura GraphQL Endpoint (something like `https://<my-project-name>.hasura.app/v1/graphql`) for both `wsLink` and `httpLink`

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
