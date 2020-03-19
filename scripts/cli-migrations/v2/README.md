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
FROM hasura/graphql-engine:<version>.cli-migrations

CMD graphql-engine \
  --database-url $DATABASE_URL \
  serve \
  --server-port $PORT \
  --enable-console
```

### Local

This is covered in the documentation here: https://hasura.io/docs/1.0/graphql/manual/migrations/auto-apply-migrations.html
The below [Configuration](#configuration) will also be applicable.

### Heroku

Using a docker build on heroku the manifest (`heroku.yml`) can look like:

```yaml
setup:
  addons:
    - plan: heroku-postgresql
      as: DATABASE
  config:
    HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR: DATABASE_URL
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

### Database (One of them required)

The following are listed in order of evaluation, therefore if setting the first, the latter will not be evaluated.
At least one of these **must** be configured to migrate the database successfully.

- `HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR` (default=`null`)

  Defines a pointer to an environment variable, which holds the database url e.g.

  `HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR=DATABASE_URL`

- `HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL` (default=`null`)

  Defines the database url for migrations, allowing it to be separate from the database used for querying.
  An example use case is for a read only follower database used for querying your graphql endpoint. In this
  scenario you would migrate only the master and the follower would be kept in sync.

  `HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>`

- `HASURA_GRAPHQL_DATABASE_URL` (default=`null`)

  Use as above to point to the default database url.

  `HASURA_GRAPHQL_DATABASE_URL=postgres://<username>:<password>@<host>:<port>/<database_name>`

### GraphQL Server (Optional)

  Optional configuration for the server which boots during migrations.

- `HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT` (default=`9691`)

  Specify the port running the graphql server during execution of the migration script.
  It is advised that you do not specify a PORT that may be open e.g. 80/443 and the default should rarely require changing.

- `HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT` (default=`30s`)

  Specify the server timeout threshold.
