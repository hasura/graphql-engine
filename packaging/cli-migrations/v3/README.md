# CLI Migrations

This docker image provides a method to run migrations and metadata at docker entrypoint.
A temporary server is booted, with the migrations API allowed, securely through localhost.
Once migrations and metadata have been applied, the server will reboot in a secure mode for inbound graphql usage.

See [./docker-entrypoint.sh](docker-entrypoint.sh)

- [CLI Migrations](#cli-migrations)
  - [Examples](#examples)
    - [Local](#local)
    - [Heroku](#heroku)
      - [Provision](#provision)
      - [Deploy](#deploy)
  - [Configuration](#configuration)
    - [Migrations Directory (Optional)](#migrations-directory-optional)
    - [Metadata Directory (Optional)](#metadata-directory-optional)
    - [Database (One of required)](#database-one-of-them-required)
    - [GraphQL Server (Optional)](#graphql-server-optional)

## Examples

It is used in a docker file as:

```dockerfile
FROM hasura/graphql-engine:<version>.cli-migrations-v3

CMD graphql-engine \
  --metadata-database-url $METADATA_DATABASE_URL \
  serve \
  --server-port $PORT \
  --enable-console
```

### Local

This is covered in the documentation here: https://hasura.io/docs/latest/graphql/core/migrations/auto-apply-migrations.html
The below [Configuration](#configuration) will also be applicable.

### Heroku

Using a docker build on heroku the manifest (`heroku.yml`) can look like:

```yaml
setup:
  addons:
    - plan: heroku-postgresql
      as: DATABASE
  config:
    HASURA_METADATA_DATABASE_URL: DATABASE_URL
build:
  docker:
    web: Dockerfile
```

This allows you to provision and migrate a database entirely without intervention.

#### Provision

Setup the app, with addons and the setup steps defined in the heroku.yml

```bash
heroku create heroku-migration-tester --manifest
```

#### Deploy

```bash
export HEROKU_GIT_REMOTE=https://git.heroku.com/heroku-migration-tester.git
git init && git add .
git commit -m "first commit"
git remote add heroku HEROKU_GIT_REMOTE
git push heroku master
```

## Configuration

You may set the following environment variables to configure the way this image is run.

### Migrations Directory (Optional)

Migrations are either mounted or built into the image.

If it has been stored in a directory other than the default then it can be configured using the following:

- `HASURA_GRAPHQL_MIGRATIONS_DIR` (default=`/hasura-migrations`)

  A path to the migrations directory.

### Metadata Directory (Optional)

Metadata are either mounted or built into the image.

If it has been stored in a directory other than the default then it can be configured using the following:

- `HASURA_GRAPHQL_METADATA_DIR` (default=`/hasura-metadata`)

  A path to the metadata directory.

### GraphQL Server (Optional)

  Optional configuration for the server which boots during migrations.

- `HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT` (default=`9691`)

  Specify the port running the graphql server during execution of the migration script.
  It is advised that you do not specify a PORT that may be open e.g. 80/443 and the default should rarely require changing.

- `HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT` (default=`30s`)

  Specify the server timeout threshold.
