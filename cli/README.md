# Hasura GraphQL Engine CLI

## Installation

Download GraphQL Engine CLI bundled with Hasura CLI

- Linux/macOS
```bash
curl -L https://cli.hasura.io/install.sh | bash
``` 
- Windows: [installer](https://cli.hasura.io/install/windows-amd64)


or download using go get
```bash
go get github.com/hasura/graphql-engine/cli/cmd/hasura
```

## Usage

```bash
hasura init --directory <my-project> --endpoint <graphql-endpoint> --access-key <access-key>
cd <my-project>
hasura console
```

## Docs

Read complete docs [here](docs/hasura.md).

## Contributing

Checkout the [contributing guide](CONTRIBUTING.md)
