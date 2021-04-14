# WhatsApp Clone
The react client is a forked version of [urigo/whatsapp-client-react](https://github.com/Urigo/WhatsApp-Clone-Client-React) and the server is backed by Hasura GraphQL Engine

- Checkout the [live app](https://whatsapp-clone.demo.hasura.app/).
- Explore the backend using [Hasura
  Console](https://whatsapp-clone.hasura.app/console).

[![Edit whatsapp-clone](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/whatsapp-clone-typescript-react/react-app?fontsize=14)

## Running the app yourself

#### Deploy Postgres and GraphQL Engine on Hasura:

- Deploy GraphQL Engine on Hasura Cloud and setup PostgreSQL via Heroku:
  
  [![Deploy to Hasura Cloud](https://graphql-engine-cdn.hasura.io/img/deploy_to_hasura.png)](https://cloud.hasura.io/)

- Get the Hasura app URL (say `whatsapp-clone.hasura.app`)

- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/whatsapp-clone-typescript-react
  ```
- [Install Hasura CLI](https://hasura.io/docs/latest/graphql/core/hasura-cli/install-hasura-cli.html)
- Apply the migrations:
  ```bash
  cd hasura 
  hasura migrate apply --endpoint "https://whatsapp-clone.hasura.app"
  ```

#### Run the auth server

  ```bash
  cd auth-server
  ```

- Set the environment variables in `.env`

- Install and run the app

```bash
  npm install
  npm start
```

#### Run the react app

  ```bash
  cd react-app
  ```

- Set the environment variables in `.env`

```bash
  yarn install
```

- Modify the codegen.yml to include the correct endpoint and headers

- Generate the graphql types by running

```bash
  gql-gen
```
This would generate the required types in `src/graphql/types`

- Run the app

```bash
  yarn start
```
