import { defineConfig } from 'cypress';
import { nxE2EPreset } from '@nx/cypress/plugins/cypress-preset';

import * as customTasks from './src/support/tasks/index.ts';

type ConfigOptions = Parameters<typeof defineConfig>[0];

// The dev server used to be started by the `@nx/cypress:cypress` executor's
// `devServerTarget`; with the inferred `@nx/cypress/plugin` targets the preset
// starts it from `setupNodeEvents` instead, and waits for `baseUrl` to respond.
const nxConfig = nxE2EPreset(__filename, {
  cypressDir: 'src',
  webServerCommands: {
    default: 'npx nx run console-ce:serve',
  },
  // A cold webpack dev build of the console takes well over the 60s default.
  webServerConfig: { timeout: 5 * 60 * 1000 },
});

interface MyConfigOptions extends ConfigOptions {
  useRelativeSnapshots?: boolean;
}

const myDefineConfig = (config: MyConfigOptions) => defineConfig(config);
export default myDefineConfig({
  viewportWidth: 1440,
  viewportHeight: 900,

  chromeWebSecurity: false,
  numTestsKeptInMemory: 10,

  retries: {
    openMode: 0,
    // Allows for one automatic retry per test
    // see: https://docs.cypress.io/guides/guides/test-retries#How-It-Works
    runMode: 1,
  },

  projectId: '5yiuic',

  e2e: {
    ...nxConfig,

    // Port of the `console-ce:serve` webpack dev server. The old executor
    // injected this from the dev server; `support/getBaseUrl.ts` requires it.
    baseUrl: 'http://localhost:4200',

    video: false,

    specPattern: [
      'src/e2e/**/*test.{js,jsx,ts,tsx}',
      'src/support/**/*unit.test.{js,ts}',
    ],

    async setupNodeEvents(on, config) {
      // Keep the preset's hook: it's what starts the web server.
      await nxConfig.setupNodeEvents(on, config);

      on('task', {
        ...customTasks,
      });

      return config;
    },
  },

  useRelativeSnapshots: true,
});
