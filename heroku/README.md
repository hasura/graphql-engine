# Hasura GraphQL Engine on Heroku

## Prerequisites

- Heroku CLI
- Heroku Account
- Docker

## Setting up

- Create a Heroku App
- Create a Heroku Postgres Addon for this app
- Get the Database URL which starts with `postgres://...`
- Generate an access key, to be used as the secret key to contact Hasura GraphQL Engine
- Set config variables:
  ```bash
  heroku config:set ACCESS_KEY="<access-key>" -a "<app-name>"
  heroku config:set DATABASE_URL="<database-url-with-creds>" -a "<app-name>"
  ```

## Deploy

- Execute the following commands to deploy
  ```bash
  heroku container:push web -a <app-name>
  heroku container:release web -a <app-name>
  ```
- Check the logs to verify if everything is ok:
  ```bash
  heroku logs -a <app-name>
  ```


