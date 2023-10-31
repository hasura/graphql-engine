# Hasura GraphQL Engine CLI

[![GoDoc](https://godoc.org/github.com/hasura/graphql-engine/cli?status.svg)](https://godoc.org/github.com/hasura/graphql-engine/cli)

## Installation

### Download GraphQL Engine CLI bundled with Hasura CLI

- Linux/macOS
    ```bash
    curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | bash
    ```
    This will install the Hasura CLI in `/usr/local/bin`. You might have to provide your `sudo` password depending on the permissions of your `/usr/local/bin` location.
    
    If you’d prefer to install to a different location other than `/usr/local/bin`, set the `INSTALL_PATH` variable accordingly:
    ```bash
    curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | INSTALL_PATH=$HOME/bin bash
    ```
    
    You can also install a specific version of the CLI by providing the `VERSION` variable:
    ```bash
    curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | VERSION=v2.35.0 bash
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
make build-cli-ext copy-cli-ext
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
