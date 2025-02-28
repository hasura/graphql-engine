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

## Run v3-engine (with Postgres)

### Building with Docker

You can also start v3-engine, along with a Postgres data connector and Jaeger
for tracing using Docker:

```sh
docker compose up
```

Open <http://localhost:3000> for GraphiQL, or <http://localhost:4002> to view
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

## Run v3-engine (with DDN)

To run the engine using your existing DDN project metadata, follow these steps:

1. Generate a local build:

```bash
ddn supergraph build local
```

This command creates resolved metadata files in the `engine/build` directory.

2. Launch the required services:

```bash
ddn run docker-start
```

This command initializes the engine, connectors, and observability stack.

3. Start the engine with the generated metadata:

```bash
cargo run --bin engine -- \
    --metadata-path /path/to/project/engine/build/open_dd.json \
    --authn-config-path /path/to/project/engine/build/auth_config.json \
    --otlp-endpoint http://localhost:4317 \
    --port 3001
```

Note: Use port 3001 (or any port other than 3000) to avoid conflicts with the
engine started by `ddn run docker-start`.

### Local Development Setup

Add the following entry to your `/etc/hosts` file:

```
127.0.0.1 local.hasura.dev
```

This mapping ensures proper resolution of connector URLs by the engine.

## Running tests

To run the test suite, you need to docker login to `ghcr.io` first:

```bash
docker login -u <username> -p <token> ghcr.io
```

where `username` is your github username, and `token` is your github PAT. The
PAT needs to have the `read:packages` scope and `Hasura SSO` configured. See
[this](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-with-a-personal-access-token-classic)
for more details.

Running `just watch` will start the Docker dependencies, build the engine, and
run all the tests.

Alternatively, run the tests once with `just test`

### Updating goldenfiles

There are some tests where we compare the output of the test against an expected
golden file. If you make some changes which expectedly change the goldenfile,
you can regenerate them like this:

```sh
just update-golden-files
```

Some other tests use `insta`, and these can be reviewed with
`cargo insta review`. If the `cargo insta` command cannot be found, install it
with `cargo install cargo-insta`.

## Run benchmarks

The benchmarks operate against the reference agent using the same test cases as
the test suite, and need a similar setup.

To run benchmarks for the lexer, parser and validation:

```bash
cargo bench -p lang-graphql "lexer"
cargo bench -p lang-graphql "parser"
cargo bench -p lang-graphql "validation/.*"
```
