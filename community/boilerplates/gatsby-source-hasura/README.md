# gatsby-source-hasura

Boilerplate to get started with Gatsby, Hasura GraphQL engine as CMS and postgres as database using the awesome plugin [wyze/gatsby-source-graphql](https://github.com/wyze/gatsby-source-graphql).

# Running the app yourself

- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)
- Get the Heroku app URL (say `hasura-graphql-2.herokuapp.com`)
- Clone this repo:
  ```bash
  git clone https://github.com/hasura/gatsby-source-hasura.git
  cd gatsby-source-hasura
  ```
- [Install Hasura CLI](https://docs.hasura.io/1.0/graphql/manual/hasura-cli/install-hasura-cli.html)
- Goto `hasura/` and edit `config.yaml`:
  ```yaml
  endpoint: https://hasura-graphql-2.herokuapp.com
  ```

- Create `author` table:
  
  - Open Hasura console:
    ```bash
    hasura console
    ```

  - Navigate to `Data` section in the top nav bar and create a table as follows:

    ![Create author table](./assets/add_table.jpg)

- Insert sample data into `author` table:

  ![Insert data into author table](./assets/insert_data.jpg)

  Verify if the row is inserted successfully

  ![Insert data into author table](./assets/browse_rows.jpg)

- Run the app (go to the root of the repo):
  ```bash
  HASURA_GRAPHQL_URL=https://hasura-graphql-2.herokuapp.com/v1alpha1/graphql npm run develop
  ```
- Test the app
  Visit [http://localhost:8000](http://localhost:8000) to view the app

  ![Demo app](./assets/test_app.jpg)
