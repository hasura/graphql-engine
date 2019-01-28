# Hasura GraphQL Engine on Docker with pgAdmin

This Docker Compose setup runs [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) along with Postgres and [pgAdmin4](https://www.pgadmin.org/) using `docker-compose`.

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## Usage

- Clone this repo on a machine where you'd like to deploy graphql engine
- Edit `docker-compose.yaml` and change `DEFAULT_USER` and `DEFAULT_PASSWORD` to something secure (default pgAdmin login email/password) default value for above variables are:
    - **DEFAULT_USER:** `pgadmin@hasura.io`
    - **DEFAULT_PASSWORD:** `admin`
- Edit `docker-compose.yaml` and change `HASURA_GRAPHQL_ACCESS_KEY` to something secure
- `docker-compose up -d`

## Important endpoints
- GraphQL endpoint will be `https://<your-domain.com>:8080/v1alpha1/graphql`
- Console will be available on `https://<your-domain.com>:8080/console`
- pgAdmin will be available on `https://<your-domain.com>:5050/browser`

## Connecting to External Postgres

If you want to connect to an external/existing postgres database, replace `HASURA_GRAPHQL_DATABASE_URL` in `docker-compose.yaml` with your database url.

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**
