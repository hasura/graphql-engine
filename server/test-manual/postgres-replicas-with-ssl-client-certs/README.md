# Testing PostgreSQL read replication with SSL

This directory contains scripts that are useful to debug the handling of ssl
certificates in a non-trivial setup involving a read-replica.

There are many many ways to put together a SSL setup. Rather than forming a
single comprehensive or prescriptive setup, these are instead useful building
blocks that can be used to as a starting point for a comlex setup.

We provide a script that can generate fresh server and client certificates, and
a docker-compose file that uses these certificates to setup a database instance
and a read-replica instance. The only authentication mechanism accepted by the
databases is ssl client certificates.

Note that the ability to specify the SSL certficates to use for a data source
is an Enterprise Edition feature.

## Starting and stopping

First we need to create the certificates that will serve as the authentication
credentials for both the server and clients:

    ./make-certificates.sh

This creates the folder `certificates`, populated with a root certificate, and
client and server certificates and keys.

To start them, run the following from this directory:

    docker compose up --wait

To stop them, run:

    docker compose down

(Add `-v` to delete the data too.)

## Background on SSL in Postgres

The `./make-certificates.sh` script uses the `openssl` command line tool to
create:

* A self-signed Root Certificate Authority (CA) certificate.
* A server certificate, tied to its hostname, and signed by the root CA
* A client certificate, tied to the postgres user (`postgres`), also signed by the root CA

Depending on the connecting client's `sslmode` setting, different levels of
security is enforced, see
https://www.postgresql.org/docs/current/libpq-ssl.html#LIBPQ-SSL-PROTECTION.

`sslmode=verify-ca` and `sslmode=verify-full` requires the hostname of the
server certificate to agree with what you are actually connected to. Set
variables in `make-certificates.sh` accordingly.

## Connecting

### Ports

To connect to the databases, you will need to get the ports.

The primary port can be found by running:

    docker compose port postgres-primary 5432

The replica port can be found by running:

    docker compose port postgres-replica 5432

Note that these ports may change if you restart the server.

### Tunneling to Hasura cloud

You can use a service such as ngrok.com to tunnel your local database endpoints
onto the public internet which makes them available for e.g. a hasura cloud
instance.

### Credentials

The only way to connect with either the primary database or the read replica is using the client certificate.

### psql example

The `psql` repl tool can accept connection parameters from both the connection string and environment variables.

The below example uses environment variables to indicate the root certificate,
client certificate and client key to use to connect to the primary database:

    PGSSLCERT=certificates/client.crt \
    PGSSLKEY=certificates/client.key \
    PGSSLROOTCERT=certificates/root.crt \
    PGSSLCERTMODE=require \
    psql -a "postgresql://postgres@$(docker compose port postgres-primary 5432)"

## Experimenting with the Hasura GraphQL Engine

A bit of setup is required to get HGE to handle ssl certificates correctly.

First, create a folder, e.g. `dir-to-store-certificates`.

Then generate the certificates:

    ./make-certificates.sh

And start the test-postgres instance defined in the top-level
docker-compose.yaml file, which will serve as the metadata database, as well as
the two databases in defined in this directory's `docker-compose.yaml`.

Then run HGE as follows:

    HGE_PGSSLCERT="$(cat server/test-manual/postgres-replicas/certificates/client.crt)" \
    HGE_PGSSLKEY="$(cat server/test-manual/postgres-replicas/certificates/client.key)" \
    HGE_PGSSLROOTCERT="$(cat server/test-manual/postgres-replicas/certificates/root.crt)" \
    HASURA_GRAPHQL_ADMIN_SECRET=topsecret \
    HASURA_GRAPHQL_PG_SSL_CERTIFICATE_PATH=dir-to-store-certificates \
    cabal new-run --RTS -- \
        exe:graphql-engine-pro +RTS -N -T -s -RTS \
        serve --enable-console --console-assets-dir $PWD/frontend/dist/apps/server-assets-console-ee \
        --metadata-database-url postgres://hasura:hasura@$(docker compose port postgres 5432)/postgres

Note that while the env vars in the psql example refer to **file names**, the
env vars for HGE refer to **file contents**, populated via process-expanding of
`cat`. The double quotes is significant to preserve whitespace.

You can now add a new data source using the URL from the "psql example" section
above, together with the read-replica if you prefer, and referring to the
certificates in the `HGE_...` environment variables. The file
`sample_metadata.json` contains the metadata of a single-instance source with
ssl setup.
