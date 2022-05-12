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
- `OS` is the operating system. It could be any of these values: `debian`, `ubuntu`, `centos`
- `ARCH` is the architecture. It could be any of these values: `amd64`, `arm64`

## Devfolio Specific

### How to publish images

```
docker buildx build --build-arg BASE_IMAGE=hasura/graphql-engine:v2.6.1 --no-cache -t ghcr.io/devfolioco/graphql-engine:v0.2.6.1.cli-migrations-v3 --platform linux/amd64,linux/arm64 --push .
```
