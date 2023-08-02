# Hasura Console - Generic info

The Hasura console is an admin dashboard to manage the connected database and to try out GraphQL APIs. It is a React application bundled with webpack and the state is managed using Redux.
Served by:

1. Hasura GraphQL Engine:
   The console is served by GraphQL Engine at `/console` endpoint (when `--enable-console` flag is used). Typically runs in **No Migration Mode** which means that actions on the console are not spitting out migration “yaml” files automatically. Most users will be using the Hasura console in this mode.
2. Hasura CLI:
   Served by the Hasura CLI using `hasura console` command, typically runs with migration mode **enabled**. All the changes to schema/hasura metadata will be tracked and spit out on the filesystem as migration yaml files and a metadata yaml file. This allows for easy version controlling of the schema/hasura metadata.
   > To set up Hasura PRO CLI development environment, follow the steps mentioned
   > [here](./docs/setup-hasura-pro-cli.md).

## Contributing to Hasura console

This guide is for setting-up the console for development on your own machine, and how to contribute.

### Console issues in the repo

Issues in the repo for the console UI are labelled as `c/console`(see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fconsole)). Issues also labelled as `good first issue` are aimed at those making their first contribution to the repo (see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fconsole+label%3A%22good+first+issue%22)). Others marked as `help wanted` are those requiring community contributions on priority (see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fconsole+label%3A%22help+wanted%22)).
Please note that some of these issues, labelled with both `c/console` and `c/server`, are part of a change/task that requires modifications in both the server and the console.
Feel free to open pull requests to address these issues or to add/fix console features, even if a corresponding issue doesn't exist. If you are unsure about whether to go ahead and work on something like the latter, please get in touch with the maintainers in the `GraphQL Engine`->`contrib` channel in the community [Discord](https://discord.gg/vBPpJkS).

### Prerequisites

- [Node.js](https://nodejs.org/en/) (v16 aka 'Gallium')
- [nvm](https://github.com/nvm-sh/nvm) (Linux and macOS, [deeper shell integration](https://github.com/nvm-sh/nvm#deeper-shell-integration) strongly suggested) or [nvm-windows](https://github.com/coreybutler/nvm-windows) (Windows)
- [Hasura GraphQL Engine](https://hasura.io/docs/latest/graphql/core/getting-started/index.html)
- [Hasura CLI](https://hasura.io/docs/latest/graphql/core/hasura-cli/install-hasura-cli.html) (for working with migrations)

### Set up and install dependencies

- Fork the repo on GitHub.
- Clone your forked repo: `git clone https://github.com/<your-username>/graphql-engine`

```bash
git clone https://github.com/<your-user-name>/graphql-engine
cd graphql-engine
cd console
nvm use
yarn install
yarn nx build-server-assets console-ce
```

At this point you need to compile and run the graphql-engine (follow the [getting started guide](../../server//CONTRIBUTING.md)), run it, and the console will be served at `http://localhost:8080`.
Alternatively, run the Console development server as described below.

### Run console development server

Hasura console can be developed in two modes, `server` or `cli` mode. If you are looking to add/tweak functionality related to migrations, check out [Develop with Hasura CLI](#develop-with-hasura-cli-cli-mode), otherwise check out [Develop with Hasura GraphQL engine](#develop-with-hasura-graphql-engine-server-mode).

Both modes require a running instance of GraphQL Engine. The easiest way to get Hasura GraphQL engine instance is by Heroku. You can get it by following the steps given in [this](https://hasura.io/docs/latest/graphql/core/getting-started/heroku-simple.html) link. Other methods to install Hasura GraphQL engine are documented [here](https://hasura.io/docs/latest/graphql/core/getting-started/index.html).

[Dotenv](https://github.com/motdotla/dotenv) is used for setting environment variables for development. In production, these environment variables are templated by the server or CLI.

#### Develop with Hasura GraphQL engine (`server` mode)

In server mode, **migrations** will be disabled and the corresponding functionality on the console will be hidden.

##### Set up `.env` file

Environment variables accepted in `server` mode:

- `NODE_ENV`: Console build environment (`development`/`production`)
- `NX_CDN_ASSETS`: Should assets be loaded from CDN (`true`/`false`)
- `NX_ASSETS_PATH`: Path to console assets
- `NX_ASSETS_VERSION`: Version of console assets being served
- `NX_ENABLE_TELEMETRY`: Whether to enable telemetry (`true`/`false`)
- `NX_URL_PREFIX`: Path at which the console is running
- `NX_DATA_API_URL`: The Hasura GraphQL engine url. (If you are running it on Heroku, it will look like https://<app-name\>.herokuapp.com, if you are running locally, it will look like http://localhost:<port\>)
- `NX_SERVER_VERSION`: Hasura GraphQL Engine server version
- `NX_CONSOLE_MODE`: In server mode, it should be `server`
- `NX_IS_ADMIN_SECRET_SET`: Is GraphQl engine configured with an admin secret (`true`/`false`)
- `NX_HASURA_CONSOLE_TYPE`: The environment where the console is running, this could be `oss`, `pro` or `cloud`

Here's an example `.env` file for `server` mode:

```
NODE_ENV=development
NX_CDN_ASSETS=true
NX_ASSETS_PATH=https://graphql-engine-cdn.hasura.io/console/assets
NX_ASSETS_VERSION=channel/stable/v1.0
NX_ENABLE_TELEMETRY=true
NX_URL_PREFIX=/
NX_DATA_API_URL=http://localhost:8080
NX_SERVER_VERSION=v1.0.0
NX_CONSOLE_MODE=server
NX_HASURA_CONSOLE_TYPE=oss
NX_IS_ADMIN_SECRET_SET=true
```

The `.env` file can be placed both at the root of the `/frontend` directory or on a per-app basis. If you put the `.env` files on a per-app-basis, please remember that also the E2E tests apps (`console-ce-e2e`, for instance) need a dedicated `.env` file because they internally launch the web server of the frontend application before launching Cypress. FYI: [here is the order Nx follows](https://nx.dev/recipes/environment-variables/define-environment-variables#setting-environment-variables) to read the `.env` files.

> The server also templates `consolePath` in `window.__env` which is the relative path of the current page (something like `/console/data/schema/public`). Using this path, the console determines the DATA_API_URL in production. You do not need to worry about this in development since you are hardcoding the value of DATA_API_URL in `.env`.

_If you're contributing to team-console i.e. console for Hasura Cloud or Hasura EE, refer to [this doc](https://github.com/hasura/lux/blob/main/docs/team-console.md)._

##### Run console development server:

Switch to the correct version of Node with [nvm](https://github.com/nvm-sh/nvm) (it's strongly suggested to activate [deeper shell integration](https://github.com/nvm-sh/nvm#deeper-shell-integration))

```bash
nvm install
nvm use
```

Run the development server

```bash
npx nx run console-ce:serve
```

#### Develop with Hasura CLI (`cli` mode)

##### Set up `.env` file in apps/console-ce

Environment variables accepted in `cli` mode:

- `NODE_ENV`: Console build environment (`development`/`production`)
- `NX_API_HOST`: Hasura CLI host. Hasura CLI runs on `http://localhost` by default.
- `NX_API_PORT`: Hasura CLI port. Hasura CLI exposes the API at `9693` by default
- `NX_CDN_ASSETS`: Should assets be loaded from CDN (`true`/`false`)
- `NX_ASSETS_PATH`: Path to console assets
- `NX_ASSETS_VERSION`: Version of console assets being served
- `NX_ENABLE_TELEMETRY`: Whether to enable telemetry (`true`/`false`)
- `NX_URL_PREFIX`: Path at which the console is running
- `NX_DATA_API_URL`: The Hasura GraphQL engine url. (If you are running it on Heroku, it will look like <app-name\>.herokuapp.com, if you are running locally, it will look like http://localhost:<port\>)
- `NX_SERVER_VERSION`: Hasura GraphQL Engine server version
- `NX_CONSOLE_MODE`: In cli mode, it should be `cli`
- `NX_ADMIN_SECRET`: the admin secret passed via the CLI
- `NX_HASURA_CLOUD_ROOT_DOMAIN`: cloud root domain, used to simulate and test Hasura Pro CLI with PAT mode Eg. lux-dev.hasura.me for local lux setup

Here's an example `.env` file for `cli` mode:

```bash
NODE_ENV=development
PORT=3000
NX_API_HOST=http://localhost
NX_API_PORT=9693
NX_CDN_ASSETS=true
NX_ASSETS_PATH=https://graphql-engine-cdn.hasura.io/console/assets
NX_ASSETS_VERSION=channel/stable/v1.0
NX_ENABLE_TELEMETRY=true
NX_URL_PREFIX=/
NX_DATA_API_URL=http://localhost:8080
NX_SERVER_VERSION=v1.0.0
NX_CONSOLE_MODE=cli
NX_ADMIN_SECRET=my-admin-secret
```

##### Run console development server:

This setup requires a Hasura CLI console server to be running.

###### Start Hasura CLI console server

Start Hasura CLI console with the same Hasura GraphQL engine url as configured for `DATA_API_URL`.

```bash
hasura console --endpoint <DATA_API_URL> --admin-secret <your-admin-secret> (optional)
```

###### Start development console server in community edition mode

```bash
npx nx run console-ce:serve
```

###### Start development storybook

```bash
npx nx run console-legacy-ce:storybook
```

### Check out the console

Visit [http://localhost:4200](http://localhost:4200) to confirm the setup.

### Make changes to the code

Make changes to the code and the console will reload automatically to reflect the new changes. Keep iterating.
When adding a new feature, it is recommended to add corresponding tests too.

You can use the [Redux DevTools Extension](https://github.com/reduxjs/redux-devtools) to inspect and debug the Redux store.
It should automatically connect to the Redux store when started in development mode.

### Linter and formatter

If you want to run a linter for all files, you can do:

```bash
nx run-many --target=lint
```

To format all files, you can run:

```bash
npx nx format:write
```

More tooling is listed [here](./libs/console/legacy-ce/src/lib/docs/dev/tooling.stories.mdx) ([storybook](https://main--614d7904644d03004addd43b.chromatic.com/?path=/story/dev-tooling--page)).

### Submitting a pull request

- All the development work happens in your own fork of the graphql-engine.
- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md#commit-messages).
- Once the changes are done, create a pull request.
- CI configured for PR will run the test suite.
- Once everything goes well, it will generate a preview Heroku app.
- The source code and the preview app will be reviewed by maintainers.

<!-- prettier-ignore-end -->

### Working with PRO console

Wiki page: https://github.com/hasura/graphql-engine-internal/wiki/Console:-Code-Sharing-Between-OSS-and-PRO
