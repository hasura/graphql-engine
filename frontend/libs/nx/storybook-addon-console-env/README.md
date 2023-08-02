# nx-storybook-addon-console-env

This library contains a [Storybook addon](https://storybook.js.org/docs/react/addons/introduction) allowing
to modify some Hasura console env variables (`window.__env`), the addon uses
[Storybook global variables](https://storybook.js.org/docs/react/essentials/toolbars-and-globals#globals)
under the hood to modify and inject console variables.

## What is it for?

### Addon

This addon adds a **Console env** tab in Storybook addons tabs.

It displays and allows to modify some console env vars (for now `isAdminSet` and `consoleType`) and reloads stories to emulate each type of console or login status.

### Stories parameter

It is possible to add parameters to stories (for now `isAdminSet` and `consoleType`) to force those
variables, otherwise, default variables values are used.

## How it works

Some console env vars (for now `isAdminSet` and `consoleType`) are stored as Storybook globals variables
and are displayed/modified thanks to this addon.

When a console env global variables is updated (through a user interaction in the UI or a story parameter):

- the URL search parameters are updates with the global variables values,
- a reload of the iFrame containing the story is triggered.

A script in `preview-body.html` will extract the variables from the URL and build the proper
`window.__env` object, then console globals are correctly initialized.

## Setup

To manipulate console env variables from Storybook UI, you need to:

- install the addon,

### Install addon

Add reference to addon in your `.storybook/main.ts` file.

```ts
const config: StorybookConfig = {
  ...
  addons: [
    ...
    'storybook-addon-console-env',
    ...
  ],
  ...
};

export default config;
```

### Testing

This addon is tested thanks to Chromatic snapshots. No interaction test has been implemented yet, as the play function cannot interact with addons control.
