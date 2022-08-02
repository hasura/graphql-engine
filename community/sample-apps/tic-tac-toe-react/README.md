# Multiplayer Tic Tac Toe

[![Edit tic-tac-toe](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/github/hasura/graphql-engine/tree/master/community/sample-apps/tic-tac-toe-react/client?fontsize=14)

This is a multiplayer tic tac toe app that uses the following components:

- Frontend
  - React
  - Apollo
- Backend
  - Hasura for GraphQL CRUD over database
  - Custom GraphQL Server with ApolloServer for custom logic

## Docker deployment

To deploy all the services, run the app using Docker Compose:

```sh
docker compose up -d --build
```

(Use `docker-compose` if you are using Docker Compose v1.)

You can access your app at http://localhost:8000
