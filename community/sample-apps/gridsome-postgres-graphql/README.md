# gridsome-postgres-graphql

Boilerplate to get started with Gridsome, Hasura GraphQL engine as CMS and postgres as database using the awesome plugin [source-graphql](https://github.com/gridsome/gridsome/tree/master/packages/source-graphql).

[![Edit gridsome-postgres-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/gridsome-postgres-graphql?fontsize=14)

![Gridsome Postgres GraphQL](https://graphql-engine-cdn.hasura.io/assets/gridsome-postgres-graphql/gridsome-postgres-graphql.png)

# Tutorial

- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

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

  ![Create foreign key for author_id column to author's id](../react-static-graphql/assets/author_fk.png)

- Now create a relationship from article table to author table by going to the Relationships tab.

- Install Gridsome CLI tool if you don't have

`npm install --global @gridsome/cli`

- Install node modules:
  ```bash
  yarn install
  ```

- Configure Gridsome to use `source-graphql` plugin and a connection GraphQL url to stitch the schema. Open the file `gridsome.config.js` and modify the plugin section to configure the GraphQL Endpoint.

```js
{
  plugins: [
    {
      use: '@gridsome/source-graphql',
      options: {
        url: 'http://localhost:8080/v1/graphql',
        fieldName: 'hasura',
        headers: {
          // Authorization: `Bearer ${process.env.AUTH_TOKEN}`,
        },
      },
    }
  ]
}
```

- Make a GraphQL query from your component

```js
<template>
  <Layout>
    <h1>Articles</h1>
    <div v-if="$page.hasura.article.length">
        <div class="articles" v-for="article in $page.hasura.article" :key="article.id">
          <p>{{ article.title }} by {{ article.author.name }}</p>
        </div>
    </div>
    <div v-else>
        <p>No articles found</p>
    </div>
  </Layout>
</template>

<page-query>
query {
  hasura {
    article {
      id
      title
      author {
        name
      }
    }
  }
}
</page-query>
```

- Run the app:
  ```bash
  yarn start
  ```
- Test the app
  Visit [http://localhost:8080](http://localhost:8080) to view the app

  ![Demo app](https://graphql-engine-cdn.hasura.io/assets/gridsome-postgres-graphql/gridsome-homepage.png)

# Contributing

Checkout the [contributing guide](../../../CONTRIBUTING.md#community-content) for more details.

