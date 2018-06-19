# Hasura GraphQL Engine using docker-compose

## Prerequisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/#install-compose)

## Deploy

- Run `docker-compose up -d` inside `docker-compose` directory to start the Hasura GraphQL Engine.

## Configure

(Assuming you have already executed `hasura init`)

- Edit `config.yaml` and add `endpoint`
  ```yaml
  endpoint: http://localhost:8080
  ```

## Console

Open the console and start exploring APIs / manage tables/views:
```bash
hasura console
```
