# Hasura GraphQL Engine on Docker with pgAdmin

This Docker Compose setup runs [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) along with Postgres and [pgAdmin4](https://www.pgadmin.org/) using `docker-compose`.

## Pre-requisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## Usage

- Clone this repo on a machine where you'd like to deploy graphql engine
- Edit `docker-compose.yaml` and change `PGADMIN_DEFAULT_EMAIL` and `PGADMIN_DEFAULT_PASSWORD` to something secure (default pgAdmin login email/password) default value for above variables are:
    - **PGADMIN_DEFAULT_EMAIL:** `pgadmin@example.com`
    - **PGADMIN_DEFAULT_PASSWORD:** `admin`
- Read more `Environment Variables` here: https://hub.docker.com/r/dpage/pgadmin4/
- Edit `docker-compose.yaml` and change `HASURA_GRAPHQL_ADMIN_SECRET` to something secure
- `docker-compose up -d`
- Navigate to `http://localhost:5050`, login and add a new server with the following parameters:  
  General - Name: Hasura  
  Connection - Host: `hasura`  
  Username: `postgres`  
  Password: leave empty  

## Important endpoints

- GraphQL endpoint will be `http://localhost:8080/v1/graphql`
- Hasura Console will be available on `http://localhost:8080/console`
- pgAdmin will be available on `http://localhost:5050`


## Connecting to External Postgres

If you want to connect to an external/existing postgres database, replace `HASURA_GRAPHQL_DATABASE_URL` in `docker-compose.yaml` with your database url.

**Note: localhost will resolve to the container ip inside a docker container, not the host ip**
