## Contributing

This guide is for setting-up the console for development on your own machine, and how to contribute.

### Pre-requisites

- Node.js (v8.9+)
- Hasura CLI (for working with migrations)
- Hasura GraphQL Engine Server

## Development Workflow

### Fork and clone

- Fork the repo on GitHub
- Clone your forked repo: `git clone https://github.com/<your-username>/graphql-engine`

### Setup

- Run `npm install` to install the modules.
- Make sure the graphql-engine server is running. It can be either local or pointing to a cloud url.
- Run `npm run build` to build the console.

### Develop

- Configure .env file according to [README.md](./README.md), where you specify environment variables for the development setup.
- Run `npm run dev` to start console in development mode.
- Run `hasura console` in case the `CONSOLE_MODE` is `cli`.

### Test

- Run tests: `npm run test`
- Write your tests in the `cypress` directory, integration.

### Create Pull Request

- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- Create a pull request from your forked repo to the main repo.
- Every pull request will automatically build and run the tests.
