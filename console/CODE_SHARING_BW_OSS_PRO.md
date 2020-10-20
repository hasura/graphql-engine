# Code Sharing Between OSS and PRO

Hasura PRO reuses all the functionality from OSS such as
- GraphiQL
- Data
- Actions
- Event Triggers
- Remote Schemas

and other utility functions. Each of the above functionality are considered a module as whole and reusing the functionality would essentially require us to do the following on the PRO side

1. Install the `router`of a particular module
2. Install the `reducer` at corresponding position in the reducer tree
3. Load the css assets on the PRO side

## Bundle

The way the code is bundled is as follows, there are multiple entrypoints for each type of module
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

The webpack config which bundles above modules is located at `./webpack/bundle.config.js`. The key thing to note here is that the config explicitely removes the external library code from the bundle as that will bloat the bundle as the code is going to be used in another source code of similar structure and similar module dependency and hence.

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

```bash
npm publish --access=public --dry-run
```

to see what will be included in the bundle

```bash
npm publish --access=public
```

to publish it
