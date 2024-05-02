# Hasura GraphQL Engine V3

[![Docs](https://img.shields.io/badge/docs-v3.x-brightgreen.svg?style=flat)](https://hasura.io/docs/3.0/index/)

Hasura V3 is the API execution engine, based over the Open Data Domain
Specification (OpenDD spec) and Native Data Connector Specifications (NDC spec),
which powers the Hasura Data Delivery Network (DDN). The v3-engine expects to
run against an OpenDDS metadata file and exposes a GraphQL endpoint according to
the specified metadata. The v3-engine needs a data connector to run alongside,
for the execution of data source specific queries.

## Data connectors

Hasura v3-engine does not execute queries directly - instead it sends IR
(abstracted, intermediate query) to NDC agents (aka data connectors). To run
queries on a database, we'll need to run the data connector that supports the
database.

Available data connectors are listed at the
[Connector Hub](https://hasura.io/connectors)

For local development, we use the reference agent implementation that is a part
of the [NDC spec](https://github.com/hasura/ndc-spec).

To start the reference agent only, you can do:

```sh
docker compose up reference_agent
```

and point the host name `reference_agent` to localhost in your `/etc/hosts`
file.

## Run v3-engine (with reference agent)

### Building locally using `cargo`

Hasura v3-engine is written in Rust, hence `cargo` is required to build and run
the v3-engine locally.

To start the v3-engine locally, we need a `metadata.json` file and an auth
config file.

Following are steps to run v3-engine with a reference agent (read only, in
memory, relational database with sample tables), and an sample metadata file,
exposing a fixed GraphQL schema. This can be used to understand the build setup
and the new V3 concepts.

```sh
RUST_LOG=DEBUG cargo run --release --bin engine -- \
  --metadata-path crates/open-dds/examples/reference.json \
 --authn-config-path auth_config.json
```

A dev webhook implementation is provided in `crates/auth/dev-auth-webhook`, that
exposes the `POST /validate-request` which accepts converts the headers present
in the incoming request to a object containing session variables, note that only
headers that start with `x-hasura-` will be returned in the response.

The dev webhook can be run using the following command:

```sh
docker compose up auth_hook
```

and point the host name `auth_hook` to localhost in your `/etc/hosts` file.

Open <http://localhost:3000> for GraphiQL.

Use `--port` option to start v3-engine on a different port.

```sh
RUST_LOG=DEBUG cargo run --release --bin engine -- \
     --port 8000 --metadata-path crates/open-dds/examples/reference.json
```

Now, open <http://localhost:8000> for GraphiQL.

## Run v3-engine (with Postgres)

### Building with Docker

You can also start v3-engine, along with a Postgres data connector and Jaeger
for tracing using Docker:

```sh
METADATA_PATH=crates/engine/tests/schema.json AUTHN_CONFIG_PATH=auth_config.json docker compose up
```

Open <http://localhost:3001> for GraphiQL, or <http://localhost:4002> to view
traces in Jaeger.

Note: you'll need to add `{"x-hasura-role": "admin"}` to the Headers section to
run queries from GraphiQL.

[NDC Postgres](https://github.com/hasura/ndc-postgres) is the official connector
by Hasura for Postgres Database. For running V3 engine for GraphQL API on
Postgres, you need to run NDC Postgres Connector and have a `metadata.json` file
that is authored specifically for your Postgres database and models (tables,
views, functions).

The recommended way to author `metadata.json` for Postgres, is via Hasura DDN.

Follow the
[Hasura DDN Guide](https://hasura.io/docs/3.0/getting-started/overview/) to
create a Hasura DDN project, connect your cloud or local Postgres Database
(Hasura DDN provides a secure tunnel mechanism to connect your local database
easily), and model your GraphQL API. You can then download the authored
metadata.json and use the following steps to run GraphQL API on your local
Hasura V3 engine.

### Steps to run metadata with V3 engine locally

1.  Download metadata from DDN project, using Hasura V3 CLI

    ```sh
    hasura3 build create --dry-run > ddn-metadata.json
    ```

2.  Following steps are to generate Postgres metadata object and run the
    Postgres Connector. These steps refer to the
    [NDC Postgres](https://github.com/hasura/ndc-postgres) repository:

    1.  Start the Postgres connector in configuration mode (Config server). A
        config server provides additional endpoints for database instrospection
        and provide the schema of the database. Output of the config server will
        form the Postgres Metadata object.
    2.  Run the following command in the
        [ndc-postgres](https://github.com/hasura/ndc-postgres) repository:

        ```bash
        just run-config
        ```

    3.  Generate the postgres configuration using the `new-configuration.sh`
        script by running the following command (in another terminal) in the
        [ndc-postgres](https://github.com/hasura/ndc-postgres) repository:

        ```bash
        ./scripts/new-configuration.sh localhost:9100 '<postgres database url>' > pg-config.json
        ```

    4.  Now shutdown the postgres config server and start the Postgres Connector
        using the `pg-config.json` generated in the above step, by running the
        following command:

        Please specify different `PORT` for different data connectors:

        ```bash
        PORT=8100 \
        RUST_LOG=INFO \
            cargo run --bin ndc-postgres --release -- serve --configuration pg-config.json > /tmp/ndc-postgres.log
        ```

    5.  Fetch the schema for the data connector object by running the following
        command:

        ```bash
        curl -X GET http://localhost:8100/schema | jq . > pg-schema.json
        ```

    6.  Finally, generate the `DataConnector` object:

        ```bash
        jq --null-input --arg name 'default' --arg port '8100' --slurpfile schema pg-schema.json '{"kind":"DataConnector","version":"v2","definition":{"name":"\($name)","url":{"singleUrl":{"value":"http://localhost:\($port)"}},"schema":$schema[0]}}' > pg-metadata.json
        ```

3.  Now you have the NDC Postgres connector running, and have obtained the
    Postgres metadata (`pg-metadata.json`) which is required for the V3 engine.

4.  In `ddn-metadata.json` (from step 1.), replace the `HasuraHubDataConnector`
    objects with `DataConnector` objects generated inside the `pg-metadata.json`
    file.

5.  Remove the object for `kind: AuthConfig` from `ddn-metadata.json`, move it
    to a separate file `auth_config.json`, and remove the `kind` field from it.

6.  Remove the object for `kind: CompatibilityConfig` from `ddn-metadata.json`.
    If desired, a `flags` field can be added to the OSS metadata to enable the
    flags corresponding to that compatibility date in the DDN metadata.

7.  Finally, start the v3-engine using the modified metadata using the following
    command (using the modified `ddn-metadata.json` and `auth_config.json` from
    Step 5):

    ```bash
    RUST_LOG=DEBUG cargo run --release --bin engine -- \
     --metadata-path ddn-metadata.json auth_config.json
    ```

    You should have the v3-engine up and running at http://localhost:3000

**Note**: We understand that these steps are not very straightforward, and we
intend to continuously improve the developer experience of running OSS V3
Engine.

## Running tests

To run the test suite, you need to docker login to `ghcr.io` first:

```bash
docker login -u <username> -p <token> ghcr.io
```

where `username` is your github username, and `token` is your github PAT. The
PAT needs to have the `read:packages` scope and `Hasura SSO` configured. See
[this](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-with-a-personal-access-token-classic)
for more details.

Next run the postgres NDC locally using `docker compose up postgres_connector`
and point the host name `postgres_connector` to localhost in your `/etc/hosts`
file.

Next run the custom NDC locally using `docker compose up custom_connector` and
point the host name `custom_connector` to localhost in your `/etc/hosts` file OR
you can run `cargo run --bin agent` and then do `cargo test`.

### Testing/Development with the chinook database

The `crates/engine/tests/chinook` contains static files required to run
v3-engine run with the chinook database as a data connector.

To get this running, you can run the following command:

```bash
METADATA_PATH=crates/engine/tests/schema.json AUTHN_CONFIG_PATH=auth_config.json docker compose up postgres_connector engine
```

If you are running the v3-engine locally through cargo, then you'll need to
update the value of the `singleUrl` present in
`crates/engine/tests/chinook/schema.json** from `"http://postgres_connector:8080"`to`"http://localhost:8100"`.

### Running tests with a single command

Alternatively, the tests can be run in the same Docker image as CI:

```sh
just test
```

### Updating goldenfiles

There are some tests where we compare the output of the test against an expected
golden file. If you make some changes which expectedly change the goldenfile,
you can regenerate them like this:

Locally (with postgres_connector pointing to localhost)

```sh
  REGENERATE_GOLDENFILES=1 cargo test
```

Docker:

```sh
  just update-golden-files
```

### Running coverage report

We can check for coverage of unit tests by running:

```sh
just coverage
```

You can also give a filter expression (which is passed to `grep -E`) to give
coverage only for matched files:

```sh
just coverage "open-dds|engine"
```

## Run benchmarks

The benchmarks operate against the reference agent using the same test cases as
the test suite, and need a similar setup.

To run benchmarks for the lexer, parser and validation:

```bash
cargo bench -p lang-graphql "lexer"
cargo bench -p lang-graphql "parser"
cargo bench -p lang-graphql "validation/.*"
```

Alternatively, the benchmarks can be run in the same Docker image as CI:

```sh
just ci-bench
```
