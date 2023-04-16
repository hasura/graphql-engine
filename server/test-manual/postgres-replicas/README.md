# Testing PostgreSQL read replication

This provides a Docker Compose file that starts up a PostgreSQL primary server and a single read replica.

We can use this to test how the HGE Server and Console behave when communicating with a read replica.

## Starting and stopping

To start them, run the following from this directory:

    docker compose up --wait

To stop them, run:

    docker compose down

(Add `-v` to delete the data too.)

## Connecting

### Ports

To connect to the databases, you will need to get the ports.

The primary port can be found by running:

    docker compose port postgres-primary 5432

The replica port can be found by running:

    docker compose port postgres-replica 5432

Note that these ports may change if you restart the server.

### Credentials

The database username is `postgres`, and the password is `password`, for both the primary and the replica.

## Experimenting with the Hasura GraphQL Engine

Run HGE as follows:

    cabal run graphql-engine:exe:graphql-engine -- \
      --metadata-database-url="postgresql://postgres:password@$(docker compose --project-directory=server/test-manual/postgres-replicas port postgres-primary 5432)" \
      serve \
      --enable-console \
      --console-assets-dir=$PWD/frontend/dist/apps/server-assets-console

Then run the following to get the replica database connection URL, and add it as the "default" data source.

    echo "postgresql://postgres:password@$(docker compose --project-directory=server/test-manual/postgres-replicas port postgres-primary 5432)"
