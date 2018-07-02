# Contributing

You can follow your existing Golang workflow to fork, work on a branch and
submit PR. If you're new to forking and working on Golang repositories, please
follow the instructions below to make sure the import paths are correct.

- Fork the repo
- `mkdir -p $GOPATH/src/github.com/hasura`
- `cd $GOPATH/src/github.com/hasura`
- `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine`
- `git remote add upstream https://github.com/hasura/graphql-engine`
- `git checkout -b <branch-name>`
- Work on the feature/fix
- Add tests and ensure all tests are passing (`make test`)
- Commit, push and submit PR

## Development workflow

We suggest using [realize](https://github.com/oxequa/realize) for faster dev
workflow. The `.realize.yaml` config is already included in the repo.

- Install realize
  ```bash
  go get github.com/oxequa/realize
  ```
- Start realize
  ```bash
  realize start
  ```

`realize` watches the directory for changes and rebuilds the cli whenever a new
change happens. The cli is installed to `$GOPATH/bin/hasura`, which should
already be in your `PATH`. The config is located at `.realize/realize.yaml`.
