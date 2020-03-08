# Hasura GraphQL Engine CLI

[![GoDoc](https://godoc.org/github.com/hasura/graphql-engine/cli?status.svg)](https://godoc.org/github.com/hasura/graphql-engine/cli)

## Installation

### Download GraphQL Engine CLI bundled with Hasura CLI

- Linux/macOS
```bash
curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | bash
```
- Windows

Download the binary `cli-hasura-windows-amd64.exe` available under Assets of the latest release from the GitHub release page: https://github.com/hasura/graphql-engine/releases

### Download using go get

```bash
go get github.com/hasura/graphql-engine/cli/cmd/hasura
```

### Build from source

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/cli
make deps
make build
# binaries will be in _output directory
```

## Usage

```bash
hasura init --directory <my-project> --endpoint <graphql-endpoint> --admin-secret <admin-secret>
cd <my-project>
hasura console
```

## Contributing

Checkout the [contributing guide](CONTRIBUTING.md).
