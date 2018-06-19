# Hasura GraphQL Engine using docker-compose

This installation method is ideal for setting up local devlopment. It will bring up both Postgres and the Hasura graphql engine.

If you want to run just the Hasura graphql engine and connect it to your own Postgres, refer to the [docker guide](../docker-run/README.md).

## Prerequisites

- [Docker](https://docs.docker.com/install/)
- [Docker Compose](https://docs.docker.com/compose/install/#install-compose)

## Install the Hasura CLI

```bash
curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash 
```

Once the download is complete, hit ctrl-c before you're prompted for your password and then move the file manually. We're doing this because this is a preview release of the hasura CLI!

```bash
mv /tmp/hasura /usr/local/bin/hasura-dev
```

## Initialise

- Initialise a Hasura project directory.
```bash
hasura init --directory my-project
```

## Bring up postgres and hasura

```bash
cd my-project
cd __install/docker-compose
docker-compose up -d
```

## Open the console

- Edit `config.yaml` and set `endpoint`
  ```yaml
  endpoint: http://localhost:8080
  ```

## Console

Open the console and start exploring APIs / manage tables/views:
```bash
hasura-dev console
```
