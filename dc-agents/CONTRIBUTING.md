# Contributing to the Data Connector Agents

## Getting Set Up
Requirements:
- NodeJS 16 - Easiest way to install is to use [nvm](https://github.com/nvm-sh/nvm), then do `nvm use` to use the correct NodeJS version via the `.nvmrc` file

Once node is installed, run `npm ci` to restore all npm packages.

## Project Structure
- `dc-api-types` - These are the TypeScript types, generated from the OpenAPI spec of the Data Connector Agent API. The OpenAPI spec is generated from our Haskell types in `/server/lib/dc-api/src`.
- `reference` - The Reference agent that serves as an example implementation of a Data Connector agent
- `sqlite` - The SQLite Data Connector agent
- `sdk` - Assets that go into the Data Connector SDK zip file
- `scripts` - Scripts used for managing the codebase

`dc-api-types`, `sqlite` and `reference` are all npm packages that are included in the root workspace defined by `package.json`. Linking them via [workspaces](https://docs.npmjs.com/cli/v7/using-npm/workspaces) means you can change the `dc-api-types` and have those changes immediately flow into the `reference` and `sqlite` packages.

To restore the npm modules used by all the projects, ensure you run `npm ci` in the `/dc-agents` directory (ie. this directory).

### Deriving lockfiles
Because `sqlite` and `reference` are linked into the root workspace, they don't normally get their own lockfiles (ie. `package-lock.json`), as the lockfile is managed at the root workspace level. However, we want to be able to take these projects and build them outside of the workspace setup we have here, where the root `package-lock.json` does not exist.

In order to achieve this, we have a tool that will derive individual `package-lock.json` files for the `reference` and `sqlite` packages from the root `package-lock.json` file. These derived `package-lock.json` files are committed to the repository so that they can be used by the package-specific Dockerfiles (eg `reference/Dockerfile`).

This means that whenever you modify the root `package-lock.json` (which will happen whenever you change the dependencies in any of the packages), you need to re-derive the individual packages' `package-lock.json` files. You can do that easily by running

```bash
> make derive-lockfiles
```

There is more information about how this derivation process works inside the script that does the derivation (`scripts/derive-lockfile.ts`).

### Docker
There are actually two Dockerfiles for each agent; for example there is both `Dockerfile-reference`, and `reference/Dockerfile`.

`Dockerfile-reference` builds a Docker container that will run the Reference agent, by copying the root workspace into the container and maintaining the workspace structure inside the container. This is useful if you need a Docker container with a Reference agent that is using as-of-yet unpublished (to npm) changes in the `dc-api-types` package, as it will include `dc-api-types` into the Docker container. This dockerfile is good to use while developing.

On the other hand, `reference/Dockerfile` will build a Docker container that will run the Reference agent, but it builds the agent independent of the workspace, so it will try to restore the `dc-api-types` package from npm (via the derived `reference/package-lock.json` file). This is good for official releases of the Reference agent, where all dependencies have been published to npm already and are available for package restore.

## Running the agents
Ensure you have run `npm ci` before doing the following.

### Reference agent
```bash
> make start-reference-agent
```

### SQLite agent
```bash
> make start-sqlite-agent
```

## Generating the TypeScript types (`dc-api-types`)
To regenerate the TypeScript types from the Haskell types, run

```bash
> make regenerate-types
```
This will regenerate the types, bump the version number in the `dc-api-types` project, update the agents to use the new version number, and update and re-derive all the lockfiles.

To only (re)generate the TypeScript types from the OpenAPI spec (ie. `dc-api-types/src/agent.openapi.json`), run

```bash
> make generate-types
```

If you need to manually change the version number in the `dc-api-types` project, you can update all dependencies to use the new version automatically by running

```
> make update-api-types-deps
```

## Publishing the TypeScript types package (`dc-api-types`) to npm
The TypeScript types package in `dc-api-types` will be published to npm by the continuous integration build system on every commit to main. It will only publish the package if the version specified in `dc-api-types/package.json` hasn't already been published.
