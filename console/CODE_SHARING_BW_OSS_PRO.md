# Code Sharing Between OSS and PRO

Hasura PRO reuses all the functionality from OSS such as
- GraphiQL
- Data
- Actions
- Event Triggers
- Remote Schemas

and other utility functions. Each of the above functionality are considered a module (require attaching it to `router` and `reducer` and they are good to go) as a whole and reusing the functionality would essentially require us to do the following on the PRO side

1. Install the `router`of a particular module
2. Install the `reducer` at corresponding position in the reducer tree
3. Load the css assets on the PRO side

## OSS Changes

Few changes are made on the OSS source code that essentially enable `PRO` or `CLOUD` code to enforce certain access control. For example:
1. `requireAsyncGlobals` in `App/Actions` is changed as follows

  **Change**: extend the function signature to accept two flags to enable each fetch as described below with default to true for it to work with OSS

  The function fetches the server config by pinging `/v1alpha1/config` and retrieves the hasura `server` metadata stored in the user's graphql engine using `/v1/query`
  1. Server config will only be live for users with atleast `graphql` access or the `admin`
  2. Server metadata would require the collaborator to have atleast `admin` access as this queries `/v1/query` and that is only allowed for `admins` for now
  `boolean` values will be passed accordingly based on the collaborator access.
2. **Change**: Modifies `initQueries` to filter schema tables/view based on certain parameters passed by `PRO`/`CLOUD` to the function `dataRouterUtils`.
3. **Change**: Adds `ADMIN_SECRET_HEADER_KEY` to constants at `cypress/helper` and update the imports accordingly. The constant was resolved from `src/` before and it was causing some trouble while sharing the tests with `pro`.


## Bundle

An oss package build would comprise of following files each representing a webpack bundle
1. hoc
  - routers
  - reducers
2. main
  - contains all the adhoc functions, utilities etc
3. app
  - exports off the app component which OSS uses
4. constants
  - exports content of constants
5. appState
  - exports off app state utilities

### Config

The webpack config which bundles above modules is located at `./webpack/bundle.config.js`. The key thing to note here is that the config explicitely removes the external library code from the bundle as that will bloat the bundle as the code is going to be used in another source code of similar structure and similar module dependency.

```
16: externals: [nodeExternals()]
```

### Bundling

Run the following command to initiate the bundling process

```bash
npm run build-lib
```

The above will update the `lib` directory with all the modules we spoke about


### Publish

#### Prerequisites

Modules are published to an npm account owned by `accounts@hasura.io` and the modules are scoped with `@hasura`

```bash
npm publish --dry-run
```

to see what will be included in the bundle

```bash
npm publish
```

to publish it
