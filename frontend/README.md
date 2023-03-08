# Frontend

This project was generated using [Nx](https://nx.dev).

Visit the [Nx Documentation](https://nx.dev) to learn more about it.

This `frontend` monorepo contains the Hasura Console, in all the possible modes.

_Last import [2023-01-12](https://github.com/hasura/graphql-engine-mono/tree/a8cbb297437e4c2d9ba4cab5da1e464d4eac43e4)_

## Nx Console

It's highly recommended to install the Nx Console for [VSCode](https://marketplace.visualstudio.com/items?itemName=nrwl.angular-console), [IntelliJ](https://plugins.jetbrains.com/plugin/15101-nx-console-idea) or [Neovim](https://github.com/Equilibris/nx.nvim).

It will allow you to have all of the Nx commands usable via UI, have an embedded view of the NX graph right in your editor, and much more.

## Hasura Console

The Hasura Console is an admin dashboard to manage the connected database and to try out GraphQL APIs. It is a React application bundled with Webpack, and the state is managed mostly using Redux.

### Table of contents

- [Generic info](./docs/generic-info.md)
- [Coming from the previous codebase](./docs/from-previous-console.md)
- [Development Tooling](https://main--614d7904644d03004addd43b.chromatic.com/?path=/story/dev-tooling--page)
- [Design System's Storybook](https://main--614d7904644d03004addd43b.chromatic.com)
  - [How to create/document new Components](./libs/console/legacy-ce/src/lib/docs/dev/ComponentDoc.stories.mdx)
- Cypress Dashboard
  - [CE Console](https://dashboard.cypress.io/projects/5yiuic)
  - [EE Console](https://dashboard.cypress.io/projects/672jmv)
- [Cypress README](./cypress/README.md)

## How to

### Install the dependencies

Run `npm install`.

### Development server

Run `nx serve console-ce` (requires an [`.env` file](./docs/generic-info.md#set-up-env-file)) for a dev server. Navigate to http://localhost:4200/. The app will automatically reload if you change any of the source files.

### Build

Run `nx build console-ce` to build the project. The build artifacts will be stored in the `dist/` directory.

## Running unit tests

Run `nx test console-ce` to execute the unit tests via [Jest](https://jestjs.io).

Run `nx affected:test` to execute the unit tests affected by a change.

## Running end-to-end tests

Run `nx e2e console-ce-e2e --watch` (requires an [`.env` file](./docs/generic-info.md#set-up-env-file)) to open the Cypress UI and locally working with it.

Run `nx e2e console-ce-e2e` (requires an [`.env` file](./docs/generic-info.md#set-up-env-file)) to execute the end-to-end tests via [Cypress](https://www.cypress.io).

Run `nx affected:e2e` (requires an [`.env` file](./docs/generic-info.md#set-up-env-file)) to execute the end-to-end tests affected by a change.

## Understand your workspace

Run `nx graph` to see a diagram of the dependencies of your projects.
