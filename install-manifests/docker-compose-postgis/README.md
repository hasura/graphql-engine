# Hasura GraphQL Engine on Docker

This Docker Compose setup runs [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) along with Postgres with the postgis extension using `docker-compose`.

See [this blog post for a tutorial](https://blog.hasura.io/graphql-and-geo-location-on-postgres-using-hasura-562e7bd47a2f).

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## Usage

- Clone this repo on a machine where you'd like to deploy graphql engine
- `docker-compose up -d`

GraphQL endpoint will be `https://<your-domain.com>/v1/graphql`
Console will be available on `https://<your-domain.com>/console`

## Connecting to External Postgres

If you want to connect to an external/existing postgres database, replace `HASURA_GRAPHQL_DATABASE_URL` in `docker-compose.yaml` with your database url. 

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**


