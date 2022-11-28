# Coming from the previous /console & /pro/console codebase

This section of the doc is aimed for engineers that comes from the old codebase into this new workspace.

## Where is my code ?

Before, we had /console and /pro/console, and now we have 2 libraries and 2 apps.

<details>
<summary>What's the difference between apps and libs ?</summary>

  > A common mental model is to see the application as "containers" that link, bundle and compile functionality implemented in libraries for being deployed. As such, if we follow a 80/20 approach:
  > * place 80% of your logic into the libs/ folder
  > * and 20% into apps/
  > Note, these libraries don’t necessarily need to be built separately, but are rather consumed and built by the application itself directly. Hence, nothing changes from a pure deployment point of view.
  
From the [Nx](https://nx.dev/more-concepts/applications-and-libraries) docs.

</details>

For now, we will only work in the `legacy-ce` and the `legacy-ee` while we plan out a proper strategy to create libs.

In the `libs/console/legacy-ce` library, there is the whole old `/console` codebase. Everything that was present under the `/console/src` folder is now in the `libs/console/legacy-ce/src/lib/` folder.

```
apps
  console-ce => empty shell (for now) that contains the globals css for the ce console
  console-ce-e2e => cypress tests for the ce console
  console-ee => empty shell (for now) that contains the globals css for the ee console
libs
  console
    legacy-ce => main library for the ce console
      src
        lib => 1:1 mapping from the old /console/src folder
        exports => 1:1 mapping from the old /console/exports folder
       index.ts => entrypoint for the library
    legace-ee => main library for the ee console
      src
      	lib => 1:1 mapping from the old /pro/console folder
        index.ts => entrypoint for the library
```

## How do I do env config

The location and names of environment variables are being stored is in a new place in the NX workspace.

You can add a `.env` in `apps/console-ce/.env` & `apps/console-ee/.env` or at the root of the NX workspace.

However, one big change is that now all env variables **must be prefixed by `NX_`** for the local dev. This has no impact on the production config.

<details>
  <summary>Example of .env with the new names</summary>

```
NODE_ENV=development
NX_PORT=4200
NX_CDN_ASSETS=true
NX_ASSETS_PATH=https://graphql-engine-cdn.hasura.io/console/assets
NX_ASSETS_VERSION=channel/stable/v1.0
NX_ENABLE_TELEMETRY=true
NX_URL_PREFIX=/
NX_DATA_API_URL=http://localhost:8080
NX_SERVER_VERSION=v1.0.0
NX_CONSOLE_MODE=server
NX_IS_ADMIN_SECRET_SET=true
```

</details>

If you want to add new variables, you will need to modify the index.html either at `apps/console-ce/src/index.html` or `apps/console-ee/src/index.html`, depending on what console you want to change.
