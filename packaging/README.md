# Packaging

All things related to packaging graphql-engine.

## hasura/graphql-engine

([Docker hub](https://hub.docker.com/r/hasura/graphql-engine))

Production ready builds of the Hasura graphql-engine.

Supported tags:
- `<version>`, `latest`
- `<version>.cli-migrations-v2`, `latest.cli-migrations-v2`
- `<version>.cli-migrations-v3`, `latest.cli-migrations-v3`

## hasura/graphql-engine-base

([Docker hub](https://hub.docker.com/r/hasura/graphql-engine-base))

This acts as base image for building `hasura/graphql-engine` image.

It is generally published with format `hasura/graphql-engine-base:<SHA256>.<OS>.<ARCH>` where
- `<SHA256>` is the `sha256sum` of the dockerfile used to build the image
- `OS` is the operating system. It could be any of these values: `ubuntu`
- `ARCH` is the architecture. It could be any of these values: `amd64`, `arm64`