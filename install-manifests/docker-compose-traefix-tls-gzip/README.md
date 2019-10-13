# Hasura GraphQL Engine on Docker with Traefix (Reverse Proxy and Load Balancer)

This Docker Compose setup runs [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) along with Postgres, Platypus (authentication), and Traefix (a reverse proxy and load balancer, it's officially classified as an edge router) using `docker-compose`.

The platypus and Hasura engine will be both serving with TLS enabled behind _Traefix_. They will have HSTS (TLS Security headers) enabled and gzip compression enabled, and CORS enabled. These are confiurable from the `labels` section in the `docker-compose.yml`.

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## Usage

- Clone this repo on a machine where you'd like to deploy graphql engine
- Edit the environment variables in the `.env` file.
- Add an `acme.json` in your home directory (`~`) and leave it empty. This can be done with `touch ~/acme.json`. Feel free to move this volume elsewhere to your liking.
- `docker-compose up -d`

To view logs: `docker-compose logs -f`.

GraphQL endpoint will be `https://<your-domain.com>/v1/graphql`
Console will be available on `https://<your-domain.com>/console`

## Connecting to External Postgres

If you want to connect to an external/existing postgres database, replace `HASURA_GRAPHQL_DATABASE_URL` in `docker-compose.yaml` with your database url.

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**
