# Hasura GraphQL Engine using Docker

## Prerequisites

- Docker

## Setting up

- Start a Postgres container:
  ```bash
  docker run --name hasura-postgres -e POSTGRES_PASSWORD=mysecretpassword -d postgres
  ```

## Deploy

- Start Hasura GraphQL Engine container, expose it at port 9000 and link to the running Postgres container:
  ```bash
  docker run --name hasura-graphql-engine \
             -p 9000:9000 \
             --link hasura-postgres:postgres -d \
             hasuranightly/raven:94a0141 \
             raven \
             --database-url postgres://postgres:mysecretpassword@postgres:5432/postgres \
             serve \
             --server-port 9000 \
             --cors-domain "http://localhost:9695"
  ```

## Configure

(Assuming you have already executed `hasura init`)

- Edit `config.yaml` and add `endpoint`:
  ```yaml
  endpoint: http://localhost:9000
  ```

## Console

Open the console and start exploring APIs / manage tables/views:
```bash
hasura console
```
