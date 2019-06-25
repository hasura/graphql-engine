# WhatsApp Clone
The react client is a forked version of [urigo/whatsapp-client-react](https://github.com/Urigo/WhatsApp-Clone-Client-React) and the server is backed by Hasura GraphQL Engine

- Checkout the [live app](https://whatsapp-clone.demo.hasura.app/).
- Explore the backend using [Hasura
  Console](https://whatsapp-clone.demo.hasura.app/console).

[![Edit whatsapp-clone](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/whatsapp-clone-typescript-react/react-app?fontsize=14)

## Running the app yourself

#### Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)
- Get the Heroku app URL (say `whatsapp-clone.herokuapp.com`)
- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/whatsapp-clone-typescript-react
  ```
- [Install Hasura CLI](https://docs.hasura.io/1.0/graphql/manual/hasura-cli/install-hasura-cli.html)
- Apply the migrations:
  ```bash
  cd hasura 
  hasura migrate apply --endpoint "https://whatsapp-clone.herokuapp.com"
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
