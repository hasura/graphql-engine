# quasar-framework-vue-graphql

A boilerplate to get started with Quasar Framework, Hasura GraphQL Engine as a CMS and Postgres as a database using the [quasar-cli](https://quasar-framework.org/guide/app-installation.html) and [vue-apollo](https://github.com/Akryum/vue-apollo) module.

[![Edit quasar-framework-vue-graphql](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/quasar-framework-vue-graphql?fontsize=14)

# Tutorial

- Deploy the GraphQL Engine on Hasura Cloud and setup PostgreSQL via Heroku:

  [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/signup)

- Get the Hasura app URL (say `quasar-graphql.hasura.app`)

- Create the `author` table:

  Open Hasura console: visit https://quasar-graphql.hasura.app on a browser
  Navigate to the `Data` section in the top nav bar and create a table as follows:

  ![Create author table](../gatsby-postgres-graphql/assets/add_table.jpg)

- Insert sample data into the `author` table:

  ![Insert data into author table](../gatsby-postgres-graphql/assets/insert_data.jpg)

  Verify if the row is inserted successfully:

  ![Insert data into author table](../gatsby-postgres-graphql/assets/browse_rows.jpg)

- Similarly, create an `article` table with the following data model:
  table: `article`
  columns: `id`, `title`, `content`, `author_id` (foreign key to `author` table's `id`)

- Clone this repo:

  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/quasar-framework-vue-graphql
  ```

- Install node modules:

  ```bash
  yarn install
  ```

- Open `src/apollo/index.js` and configure Hasura's GraphQL Endpoint as follows:

```
[...]

    link: createHttpLink({
        uri:
          process.env.GRAPHQL_URI ||
          // Change to your graphql endpoint.
          "<your-Hasura-app-endpoint>",
      }),

[...]
```

Find the above piece of code and replace `<your-Hasura-app-endpoint>` with the URL of your Hasura Cloud Project.

- We have defined the GraphQL query for fetching author list in `src/layouts/MainLayout.vue`.

  - GraphQL query

  ```graphql
    const { result, loading, error } = useQuery(gql`
      query {
        author {
          id
          name
        }
      }
    `);
  ```

  - In `pages/Articles.vue`, we have defined a GraphQL query for articles:

    ```js
     <script>
    import { watch } from "vue";
    import { useQuery, useResult } from "@vue/apollo-composable";
    import { useRoute } from "vue-router";
    import gql from "graphql-tag";

    export default {
      name: "PageName",

      setup() {
        const route = useRoute();

        const { result, loading, error, refetch } = useQuery(
          gql`
            query articleQuery($authorId: Int!) {
              article(where: { author_id: { _eq: $authorId } }) {
                id
                title
                content
              }
            }
          `,
          {
            authorId: route.params.authorId,
          }
        );

        const articles = useResult(result, null, (data) => data.article);

        watch(
          () => route.params.authorId,
          async (newId) => {
            refetch({ authorId: newId });
          }
        );

        return {
          articles,
        };
      },
    };
    </script>
    ```

- Run the app:
  ```bash
  quasar dev
  ```
- Test the app
  Visit [http://localhost:8080](http://localhost:8080) to view the app

For a detailed explanation on how things work, check out the [Quasar Framework documentation](https://quasar-framework.org/guide/).
