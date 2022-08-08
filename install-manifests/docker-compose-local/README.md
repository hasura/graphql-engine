# Hasura GraphQL Engine on Docker

This Docker Compose setup runs the [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) [auto-applying migration and metadata image](https://hasura.io/docs/latest/migrations-metadata-seeds/auto-apply-migrations/), along with Postgres, using `docker-compose`.

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)
- [Hasura CLI](https://hasura.io/docs/latest/hasura-cli/index/)

## Usage\

```bash
hasura init hasura

docker compose up -d

hasura console --project hasura
```

GraphQL endpoint will be `http://localhost:8080/v1/graphql`
Console will be available on `http://localhost:8080/console`

## Connecting to External Postgres

If you want to connect to an external/existing Postgres database, replace `PG_DATABASE_URL` in `docker-compose.yaml` with your database URL.

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**
