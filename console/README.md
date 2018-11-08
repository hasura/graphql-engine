<!-- prettier-ignore-start -->

# Hasura Console

The Hasura console is an admin dashboard to manage the connected database and to try out GraphQL APIs. It is a React application bundled with webpack and the state is managed using Redux.

Served by

1. Hasura GraphQL Engine:
   The console is served by GraphQL Engine at `/console` endpoint (when `--enable-console` flag is used). Typically runs in **No Migration Mode** which means that actions on the console are not spitting out migration “yaml” files automatically. Most users will be using the Hasura console in this mode.

2. Hasura CLI:
   Served by the Hasura CLI using `hasura console` command, typically runs with migration mode **enabled**. All the changes to schema/hasura metadata will be tracked and spit out on the filesystem as migration yaml files and a metadata yaml file. This allows for easy version controlling of the schema/hasura metadata.

## Contributing to Hasura console

This guide is for setting-up the console for development on your own machine, and how to contribute.

### Prerequisites

- Node.js (v8.9+)
- [Hasura GraphQL Engine](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html)
- [Hasura CLI](https://docs.hasura.io/1.0/graphql/manual/hasura-cli/install-hasura-cli.html) (for working with migrations)

### Setup and Install Dependencies

- Fork the repo on GitHub.
- Clone your forked repo: `git clone https://github.com/<your-username>/graphql-engine`

```bash
git clone https://github.com/<your-user-name>/graphql-engine
cd graphql-engine
cd console
npm install
```

Hasura console can be developed in two modes (`server` or `cli` mode). Both modes require a running instance of GraphQL Engine. The easiest way to get Hasura GraphQL engine instance is by Heroku. You can get it by following the steps given in [this](https://docs.hasura.io/1.0/graphql/manual/getting-started/heroku-simple.html) link. Other methods to install Hasura GraphQL engine are documented [here](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

### Development with Hasura GraphQL Engine (`server` mode)

Hasura GraphQL engine should be running to develop console in this mode. If you have set it up on heroku, your url will look like `<app-name>.herokuapp.com`, if it's on your local machine, it's probably `http://localhost:8080`.

[Dotenv](https://github.com/motdotla/dotenv) is used for setting environment variables for development. Create a `.env` file in the root directory for console (wherever package.json is). Sample `.env` file will look like below

Sample environment variables:

```bash
PORT=3000
NODE_ENV=development
DATA_API_URL=http://localhost:8080
ACCESS_KEY=xyz
CONSOLE_MODE=server
URL_PREFIX=/
```

Note that `CONSOLE_MODE` is set to `server`. In this mode, **migrations** will be disabled and the corresponding functionality on the console will be hidden. If you are looking to add/tweak functionality related to migrations, check out [Development with Hasura CLI](#development-with-hasura-cli-cli-mode).

Environment variables accepted in `server` mode:

1. `PORT`: Configure the port where Hasura console will run locally.
2. `NODE_ENV`: `development`
3. `DATA_API_URL`: Configure it with the Hasura GraphQL Engine url. If you are running it on Heroku. Your url will look like <app-name>.herokuapp.com.
4. `ACCESS_KEY`: Set access key if Hasura GraphQL engine is configured to run with ACCESS_KEY.
5. `CONSOLE_MODE`: `server`
6. `URL_PREFIX`: ‘/’ (forward slash)

#### Run Development Server:

```bash
 npm run dev
```

### Development with Hasura CLI (`cli` mode)

Configure .env file with appropriate values for the required environment variables.

Sample environment variables:

```bash
PORT=3000
NODE_ENV=development
DATA_API_URL=http://localhost:8080
API_HOST=http://localhost
API_PORT=9693
ACCESS_KEY=xyz
CONSOLE_MODE=cli
URL_PREFIX=/
```
Environment variables accepted in `cli` mode:

1. `PORT`: Configure the port where Hasura console will run locally.
2. `NODE_ENV`: `development`
3. `DATA_API_URL`: Configure it with the Hasura GraphQL Engine url. If you are running it on Heroku. Your url will look like <app-name>.herokuapp.com
4. `API_HOST`: Hasura CLI host. Hasura CLI runs on `http://localhost` by default.
5. `API_PORT`: Hasura CLI port. Hasura CLI exposes the API at `9693` by default
6. `ACCESS_KEY`: Set access key if Hasura GraphQL engine is configured to run with ACCESS_KEY
7. `CONSOLE_MODE`: `cli`
8. `URL_PREFIX`: ‘/’ (forward slash)

#### Run Development Server:

This setup requires hasura cli to be running in a different window. Start hasura cli with the same Hasura GraphQL engine url as configured for `DATA_API_URL`.

##### Start Hasura CLI server

```bash
hasura console
```

##### Start development server

```bash
npm run dev
```

### Checkout the console

Visit [http://localhost:3000](http://localhost:3000) to confirm the setup.

![Testing Development Server](../assets/console-readme-assets/test-dev-setup.jpg)

### Make changes to the code

Make changes to the code and the console will reload automatically to reflect the new changes. Keep iterating.
When adding a new feature, it is recommended to add corresponding tests too.

Tests are written using [Cypress](https://www.cypress.io/).

You can use the [Redux DevTools Extension](http://extension.remotedev.io/) to inspect and debug the Redux store.
It should automatically connect to the Redux store when started in development mode.

### Run Tests

- Run tests: `npm run cypress`
- Write your tests in the `cypress` directory, integration.

### Submitting a pull request

- All the development work happens in your own fork of the graphql-engine.
- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md#commit-messages).
- Once the changes are done, create a pull request.
- CI configured for PR will run the test suite.
- Once everything goes well, it will generate a preview heroku app.
- The source code and the preview app will be reviewed by maintainers.

<!-- prettier-ignore-end -->
