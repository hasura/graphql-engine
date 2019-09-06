# Hasura GraphQL Engine on Docker with HTTPS

This Docker Compose setup runs [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) with [Caddy](https://caddyserver.com) webserver and automatic HTTPS certs from [LetsEncrypt](https://letsencrypt.org/).

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## Usage

- Clone this repo on a machine with a public ip address
- Map your domain name to this ip address
- Edit `Caddyfile` and add your domain (replace `<your-domain.com>` with your domain, don't keep `<>`)
- Edit `docker-compose.yaml` and change `HASURA_GRAPHQL_ADMIN_SECRET` to something secure
- `docker-compose up -d`

GraphQL endpoint will be `https://<your-domain.com>/v1/graphql`
Console will be available on `https://<your-domain.com>/console`

## Connecting to External Postgres

If you want to connect to an external/existing postgres database, replace `HASURA_GRAPHQL_DATABASE_URL` in `docker-compose.yaml` with your database url. 

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**


