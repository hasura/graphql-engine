# realtime-location-app

A demo application to showcase real-time capabilities of [Hasura GraphQL
Engine](https://github.com/hasura/graphql-engine).

[![Edit realtime-location-tracking](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/realtime-location-tracking?fontsize=14)

The Realtime location application is built using React and is powered by Hasura
GraphQL Engine over Postgres. It has an interface for users to track location of a vehicle using Hasura live queries, in real-time.

The application makes use of Hasura GraphQL Engine's real-time capabilities
using `subscription`. There is no backend code involved. The application is
hosted on GitHub pages and the Postgres+GraphQL Engine is running on Postgres.

- Checkout the [live app](https://realtime-location-tracking.demo.hasura.app/).
- Explore the backend using [Hasura
  Console](https://realtime-location-tracking.demo.hasura.app/console).
  
# Running the app yourself

- Deploy Postgres and GraphQL Engine on Heroku:
  
  [![Deploy to
  heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)
- Get the Heroku app URL (say `realtime-backend2.herokuapp.com`)
- Clone this repo:
  ```bash
  git clone https://github.com/hasura/graphql-engine
  cd graphql-engine/community/sample-apps/realtime-location-tracking
  ```
- [Install Hasura CLI](https://docs.hasura.io/1.0/graphql/manual/hasura-cli/install-hasura-cli.html)
- Goto `hasura/` and edit `config.yaml`:
  ```yaml
  endpoint: https://realtime-backend2.herokuapp.com
  ```
- Apply the migrations:
  ```bash
  hasura migrate apply
  ```
- Edit `HASURA_GRAPHQL_URL` in `src/constants.js` and set it to the
  Heroku app URL:
  ```js
  export const HASURA_GRAPHQL_URL = 'realtime-backend2.herokuapp.com/v1/graphql';
  ```
- Run the app (go to the root of the repo):
  ```bash
  npm start
  ```
