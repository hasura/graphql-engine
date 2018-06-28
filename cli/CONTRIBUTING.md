# Contributing to Hasura GraphQL Engine CLI

You can follow your existing Golang workflow to fork, work on a branch and
submit PR. If you're new to forking and working on Golang repositories, please
follow the instructions below to make sure the import paths are correct.

- Fork the repo
- `mkdir -p $GOPATH/src/github.com/hasura`
- `cd $GOPATH/src/github.com/hasura`
- `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine`
- `git checkout -b <branch-name>`
- Work on the feature/fix
- Add tests and ensure all tests are passing (`make test`)
- Commit, push and submit PR
