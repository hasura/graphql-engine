# V3 Contributing Guide

## Getting Started

### Using Docker

Start a development container (which includes NDC agents for testing):

```
docker compose run --build --rm dev_setup bash
```

### Without Docker

You will need to install some packages:

- The Rust compiler
- `protobuf-compiler`

For development, you may need to install some additional tools such as
`nextest`. See the [Dockerfile](Dockerfile).

### Building the source

If the dependencies are correctly installed as above, you should now be able to
run

```
cargo build
```

From here, you can follow the instructions in <README.md> to set up a working
server.

## Architecture

See the [architecture document](docs/architecture.md).
